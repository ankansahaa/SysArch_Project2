package RISCV.implementation.RV32P

import chisel3._
import chisel3.util._
import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._

class DoubleWidthUnit extends AbstractExecutionUnit {
  io.misa := "b01__0000__0_00000_00000_00000_10000_00000".U

  io_pc.pc_we := false.B
  io_pc.pc_wdata := 0.U
  io_data.data_req := false.B
  io_data.data_addr := 0.U
  io_data.data_be := 0.U
  io_data.data_we := false.B
  io_data.data_wdata := 0.U
  io_trap.trap_valid := false.B
  io_trap.trap_reason := TRAP_REASON.NONE

  val supported = Seq(
    RISCV_TYPE.pli_db,
    RISCV_TYPE.pli_dh,
    RISCV_TYPE.plui_dh,
    RISCV_TYPE.padd_db,
    RISCV_TYPE.padd_dh,
    RISCV_TYPE.padd_dw,
    RISCV_TYPE.padd_dbs,
    RISCV_TYPE.padd_dhs,
    RISCV_TYPE.padd_dws,
    RISCV_TYPE.psub_db,
    RISCV_TYPE.psub_dh,
    RISCV_TYPE.psub_dw,
    RISCV_TYPE.psadd_db,
    RISCV_TYPE.psadd_dh,
    RISCV_TYPE.psadd_dw,
    RISCV_TYPE.pwadd_b,
    RISCV_TYPE.pwadda_b,
    RISCV_TYPE.pwadd_h,
    RISCV_TYPE.pwadda_h
  )
  io.valid := supported.map(_ === io.instr_type).reduce(_ || _)

  val cycle = RegInit(0.U(2.W))
  val lowResult = Reg(UInt(32.W))
  val highResult = Reg(UInt(32.W))
  val busy = RegInit(false.B)

  io.stall := Mux(busy, STALL_REASON.EXECUTION_UNIT, STALL_REASON.NO_STALL)

  val rs1_addr = io.instr(19, 15)
  val rs2_addr = io.instr(24, 20)
  val rd_addr = io.instr(11, 7)
  val imm12 = io.instr(31, 20) // 12-bit immediate

  io_reg.reg_rs1 := Mux(busy, 0.U, rs1_addr)
  io_reg.reg_rs2 := Mux(busy, 0.U, rs2_addr)
  io_reg.reg_rd := Mux(busy, 0.U, rd_addr)
  io_reg.reg_write_en := !busy && cycle === 0.U
  io_reg.reg_write_data := Cat(highResult, lowResult)

  when(io.valid && !busy) {
    busy := true.B
    cycle := 1.U
  }

  when(busy) {
    def sat32(x: UInt): UInt = {
      MuxCase(
        x(31, 0),
        Seq(
          (x.asSInt > 2147483647.S) -> "h7fffffff".U,
          (x.asSInt < -2147483648.S) -> "h80000000".U
        )
      )
    }
    def wordAdd8(a: UInt, b: UInt): UInt = {
      (0 until 4)
        .map(i => (a(8 * i + 7, 8 * i) +& b(8 * i + 7, 8 * i))(7, 0))
        .reduce(_ ## _)
    }
    def wordAdd16(a: UInt, b: UInt): UInt = {
      (0 until 2)
        .map(i => (a(16 * i + 15, 16 * i) +& b(16 * i + 15, 16 * i))(15, 0))
        .reduce(_ ## _)
    }

    when(cycle === 1.U) {
      val rs1_low = io_reg.reg_read_data1
      val rs2_low = io_reg.reg_read_data2
      val imm8 = imm12(7, 0)
      val imm16 = Cat(Fill(4, imm12(11)), imm12).asSInt
        .pad(32)
        .asUInt // sign-extend 12->16 then 16->32
      lowResult := MuxCase(
        0.U,
        Seq(
          (io.instr_type === RISCV_TYPE.pli_db) -> (imm8.asSInt.pad(32).asUInt),
          (io.instr_type === RISCV_TYPE.pli_dh) -> imm16,
          (io.instr_type === RISCV_TYPE.plui_dh) -> (imm12 << 16),
          (io.instr_type === RISCV_TYPE.padd_db) -> (rs1_low +& rs2_low)(31, 0),
          (io.instr_type === RISCV_TYPE.psub_db) -> (rs1_low -& rs2_low)(31, 0),
          (io.instr_type === RISCV_TYPE.psadd_db) -> sat32(rs1_low +& rs2_low),
          (io.instr_type === RISCV_TYPE.pwadd_b) -> wordAdd8(rs1_low, rs2_low),
          (io.instr_type === RISCV_TYPE.pwadda_b) -> wordAdd8(rs1_low, rs2_low),
          (io.instr_type === RISCV_TYPE.pwadd_h) -> wordAdd16(rs1_low, rs2_low),
          (io.instr_type === RISCV_TYPE.pwadda_h) -> wordAdd16(rs1_low, rs2_low)
        )
      )
      cycle := 2.U
    }.elsewhen(cycle === 2.U) {
      val rs1_high = io_reg.reg_read_data1
      val rs2_high = io_reg.reg_read_data2
      highResult := MuxCase(
        0.U,
        Seq(
          (io.instr_type === RISCV_TYPE.padd_db) -> (rs1_high +& rs2_high)(
            31,
            0
          ),
          (io.instr_type === RISCV_TYPE.psub_db) -> (rs1_high -& rs2_high)(
            31,
            0
          ),
          (io.instr_type === RISCV_TYPE.psadd_db) -> sat32(
            rs1_high +& rs2_high
          ),
          (io.instr_type === RISCV_TYPE.pwadd_b) -> wordAdd8(
            rs1_high,
            rs2_high
          ),
          (io.instr_type === RISCV_TYPE.pwadda_b) -> wordAdd8(
            rs1_high,
            rs2_high
          ),
          (io.instr_type === RISCV_TYPE.pwadd_h) -> wordAdd16(
            rs1_high,
            rs2_high
          ),
          (io.instr_type === RISCV_TYPE.pwadda_h) -> wordAdd16(
            rs1_high,
            rs2_high
          )
        )
      )
      cycle := 0.U; busy := false.B
    }
  }
}
