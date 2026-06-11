package RISCV.implementation.RV32P

import chisel3._
import chisel3.util._
import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._

class SimplePackedUnit extends AbstractExecutionUnit {
  io.misa := "b01__0000__0_00000_00000_00000_10000_00000".U

  io_pc.pc_we := false.B
  io_pc.pc_wdata := io_pc.pc + 4.U
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

  val rs1_addr = io.instr(19, 15)
  val rs2_addr = io.instr(24, 20)
  val rd_addr = io.instr(11, 7)

  val isImmediate =
    io.instr_type === RISCV_TYPE.pli_b ||
      io.instr_type === RISCV_TYPE.pli_h ||
      io.instr_type === RISCV_TYPE.plui_h

  io.stall := STALL_REASON.NO_STALL
  io_pc.pc_we := io.valid
  io_reg.reg_rs1 := Mux(isImmediate, 0.U, rs1_addr)
  io_reg.reg_rs2 := Mux(isImmediate, 0.U, rs2_addr)
  io_reg.reg_rd := rd_addr
  io_reg.reg_write_en := io.valid

  def sat8(x: SInt): UInt =
    Mux(x > 127.S, "h7f".U, Mux(x < -128.S, "h80".U, x.asUInt(7, 0)))
  def sat16(x: SInt): UInt =
    Mux(x > 32767.S, "h7fff".U, Mux(x < -32768.S, "h8000".U, x.asUInt(15, 0)))
  def avg8(a: UInt, b: UInt): UInt = ((a.asSInt +& b.asSInt) >> 1).asUInt(7, 0)
  def avg16(a: UInt, b: UInt): UInt =
    ((a.asSInt +& b.asSInt) >> 1).asUInt(15, 0)

  val rs1 = io_reg.reg_read_data1
  val rs2 = io_reg.reg_read_data2
  val imm8 = io.instr(27, 20)
  val imm10 = Cat(io.instr(20), io.instr(29, 21))
  val pluiImm = (imm10.asSInt.pad(16).asUInt << 6)(15, 0)

  val byteResult = Wire(Vec(4, UInt(8.W)))
  val halfResult = Wire(Vec(2, UInt(16.W)))
  for (i <- 0 until 4) {
    val a = rs1(8 * i + 7, 8 * i)
    val b = Mux(
      io.instr_type === RISCV_TYPE.padd_bs,
      rs2(7, 0),
      rs2(8 * i + 7, 8 * i)
    )
    byteResult(i) := MuxCase(
      a,
      Seq(
        (io.instr_type === RISCV_TYPE.padd_b || io.instr_type === RISCV_TYPE.padd_bs) -> (a +& b)(
          7,
          0
        ),
        (io.instr_type === RISCV_TYPE.psub_b) -> (a -& b)(7, 0),
        (io.instr_type === RISCV_TYPE.psadd_b) -> sat8(a.asSInt +& b.asSInt),
        (io.instr_type === RISCV_TYPE.paadd_b) -> avg8(a, b)
      )
    )
  }
  for (i <- 0 until 2) {
    val a = rs1(16 * i + 15, 16 * i)
    val b = Mux(
      io.instr_type === RISCV_TYPE.padd_hs,
      rs2(15, 0),
      rs2(16 * i + 15, 16 * i)
    )
    halfResult(i) := MuxCase(
      a,
      Seq(
        (io.instr_type === RISCV_TYPE.padd_h || io.instr_type === RISCV_TYPE.padd_hs) -> (a +& b)(
          15,
          0
        ),
        (io.instr_type === RISCV_TYPE.psub_h) -> (a -& b)(15, 0),
        (io.instr_type === RISCV_TYPE.psadd_h) -> sat16(a.asSInt +& b.asSInt),
        (io.instr_type === RISCV_TYPE.paadd_h) -> avg16(a, b)
      )
    )
  }

  io_reg.reg_write_data := MuxCase(
    0.U,
    Seq(
      (io.instr_type === RISCV_TYPE.pli_b) -> Fill(4, imm8),
      (io.instr_type === RISCV_TYPE.pli_h) -> Fill(
        2,
        imm10.asSInt.pad(16).asUInt
      ),
      (io.instr_type === RISCV_TYPE.plui_h) -> Fill(2, pluiImm),
      (io.instr_type === RISCV_TYPE.padd_b) -> byteResult.asUInt,
      (io.instr_type === RISCV_TYPE.padd_bs) -> byteResult.asUInt,
      (io.instr_type === RISCV_TYPE.psub_b) -> byteResult.asUInt,
      (io.instr_type === RISCV_TYPE.psadd_b) -> byteResult.asUInt,
      (io.instr_type === RISCV_TYPE.paadd_b) -> byteResult.asUInt,
      (io.instr_type === RISCV_TYPE.padd_h) -> halfResult.asUInt,
      (io.instr_type === RISCV_TYPE.padd_hs) -> halfResult.asUInt,
      (io.instr_type === RISCV_TYPE.psub_h) -> halfResult.asUInt,
      (io.instr_type === RISCV_TYPE.psadd_h) -> halfResult.asUInt,
      (io.instr_type === RISCV_TYPE.paadd_h) -> halfResult.asUInt
    )
  )

  when(~io_reset.rst_n) {
    io_pc.pc_we := false.B
    io_reg.reg_write_en := false.B
  }
}
