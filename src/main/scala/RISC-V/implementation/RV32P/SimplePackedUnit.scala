package RISCV.implementation.RV32P

import chisel3._
import chisel3.util._
import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._

class SimplePackedUnit extends AbstractExecutionUnit {
  io.misa := "b01__0000__0_00000_00000_00000_10000_00000".U

  // Default outputs
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
    RISCV_TYPE.pli_b,
    RISCV_TYPE.pli_h,
    RISCV_TYPE.plui_h,
    RISCV_TYPE.padd_b,
    RISCV_TYPE.padd_h,
    RISCV_TYPE.padd_bs,
    RISCV_TYPE.padd_hs,
    RISCV_TYPE.psub_b,
    RISCV_TYPE.psub_h,
    RISCV_TYPE.psadd_b,
    RISCV_TYPE.psadd_h,
    RISCV_TYPE.paadd_b,
    RISCV_TYPE.paadd_h
  )
  io.valid := supported.map(_ === io.instr_type).reduce(_ || _)

  val cycle = RegInit(0.U(2.W))
  val result = Reg(UInt(32.W))
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
  io_reg.reg_write_data := result

  when(io.valid && !busy) {
    busy := true.B
    cycle := 1.U
  }

  when(busy) {
    def sat8(x: UInt): UInt =
      Mux(x.asSInt > 127.S, 127.U, Mux(x.asSInt < -128.S, 128.U, x(7, 0)))
    def avg8(x: UInt, y: UInt): UInt = (x +& y) >> 1
    def sat16(x: UInt): UInt = Mux(
      x.asSInt > 32767.S,
      32767.U,
      Mux(x.asSInt < -32768.S, 32768.U, x(15, 0))
    )
    def avg16(x: UInt, y: UInt): UInt = (x +& y) >> 1

    switch(io.instr_type) {
      is(RISCV_TYPE.pli_b, RISCV_TYPE.pli_h, RISCV_TYPE.plui_h) {
        when(cycle === 1.U) {
          // PLI.B: sign-extend 8-bit from imm12(7,0)
          val imm8 = imm12(7, 0)
          val imm16 = Cat(Fill(4, imm12(11)), imm12).asSInt
            .pad(32)
            .asUInt // sign-extend 12->16 then 16->32
          result := MuxCase(
            0.U,
            Seq(
              (io.instr_type === RISCV_TYPE.pli_b) -> (imm8.asSInt
                .pad(32)
                .asUInt),
              (io.instr_type === RISCV_TYPE.pli_h) -> imm16,
              (io.instr_type === RISCV_TYPE.plui_h) -> (imm12 << 16)
            )
          )
          cycle := 0.U; busy := false.B
        }
      }
      // Packed arithmetic (same as before, but unchanged)
      is(
        RISCV_TYPE.padd_b,
        RISCV_TYPE.padd_h,
        RISCV_TYPE.padd_bs,
        RISCV_TYPE.padd_hs,
        RISCV_TYPE.psub_b,
        RISCV_TYPE.psub_h,
        RISCV_TYPE.psadd_b,
        RISCV_TYPE.psadd_h,
        RISCV_TYPE.paadd_b,
        RISCV_TYPE.paadd_h
      ) {
        when(cycle === 1.U) {
          val rs1 = io_reg.reg_read_data1
          val rs2 = io_reg.reg_read_data2
          var tmp = 0.U(32.W)
          for (i <- 0 until 32 by 8) {
            val a = rs1(i + 7, i)
            val b = rs2(i + 7, i)
            val out8 = MuxCase(
              a,
              Seq(
                (io.instr_type === RISCV_TYPE.padd_b) -> (a +& b)(7, 0),
                (io.instr_type === RISCV_TYPE.psub_b) -> (a -& b)(7, 0),
                (io.instr_type === RISCV_TYPE.psadd_b) -> sat8(a +& b),
                (io.instr_type === RISCV_TYPE.paadd_b) -> avg8(a, b),
                (io.instr_type === RISCV_TYPE.padd_bs) -> (a +& b)(7, 0),
                (io.instr_type === RISCV_TYPE.padd_hs) -> (a +& b)(7, 0)
              )
            )
            tmp = tmp | (out8 << i)
          }
          for (i <- 0 until 32 by 16) {
            val a = rs1(i + 15, i)
            val b = rs2(i + 15, i)
            val out16 = MuxCase(
              a,
              Seq(
                (io.instr_type === RISCV_TYPE.padd_h) -> (a +& b)(15, 0),
                (io.instr_type === RISCV_TYPE.psub_h) -> (a -& b)(15, 0),
                (io.instr_type === RISCV_TYPE.psadd_h) -> sat16(a +& b),
                (io.instr_type === RISCV_TYPE.paadd_h) -> avg16(a, b),
                (io.instr_type === RISCV_TYPE.padd_bs) -> (a +& b)(15, 0),
                (io.instr_type === RISCV_TYPE.padd_hs) -> (a +& b)(15, 0)
              )
            )
            tmp = (tmp & ~(0xffff.U << i)) | (out16 << i)
          }
          result := tmp
          cycle := 0.U; busy := false.B
        }
      }
    }
  }
}
