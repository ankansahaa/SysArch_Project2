package RISCV.implementation.RV32P

import chisel3._
import chisel3.util._
import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._

class DoubleWidthUnit extends AbstractExecutionUnit {
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
    RISCV_TYPE.paadd_db,
    RISCV_TYPE.paadd_dh,
    RISCV_TYPE.paadd_dw,
    RISCV_TYPE.pwadd_b,
    RISCV_TYPE.pwadda_b,
    RISCV_TYPE.pwadd_h,
    RISCV_TYPE.pwadda_h
  )
  io.valid := supported.map(_ === io.instr_type).reduce(_ || _)

  val phase = RegInit(0.U(2.W))
  val lowSaved = RegInit(0.U(32.W))
  val highSaved = RegInit(0.U(32.W))

  val rd = io.instr(11, 7)
  val rs1 = io.instr(19, 15)
  val rs2 = io.instr(24, 20)
  val rdHigh = rd + 1.U
  val rs1High = rs1 + 1.U
  val rs2High = rs2 + 1.U

  val isImm = io.instr_type === RISCV_TYPE.pli_db ||
    io.instr_type === RISCV_TYPE.pli_dh ||
    io.instr_type === RISCV_TYPE.plui_dh
  val isScalar = io.instr_type === RISCV_TYPE.padd_dbs ||
    io.instr_type === RISCV_TYPE.padd_dhs ||
    io.instr_type === RISCV_TYPE.padd_dws
  val isAcc = io.instr_type === RISCV_TYPE.pwadda_b ||
    io.instr_type === RISCV_TYPE.pwadda_h
  val isWiden = io.instr_type === RISCV_TYPE.pwadd_b ||
    io.instr_type === RISCV_TYPE.pwadda_b ||
    io.instr_type === RISCV_TYPE.pwadd_h ||
    io.instr_type === RISCV_TYPE.pwadda_h

  def sat32(x: SInt): UInt =
    Mux(
      x > 2147483647.S,
      "h7fffffff".U,
      Mux(x < -2147483648L.S, "h80000000".U, x.asUInt(31, 0))
    )
  def sat8(x: SInt): UInt =
    Mux(x > 127.S, "h7f".U, Mux(x < -128.S, "h80".U, x.asUInt(7, 0)))
  def sat16(x: SInt): UInt =
    Mux(x > 32767.S, "h7fff".U, Mux(x < -32768.S, "h8000".U, x.asUInt(15, 0)))
  def avg8(a: UInt, b: UInt): UInt = ((a.asSInt +& b.asSInt) >> 1).asUInt(7, 0)
  def avg16(a: UInt, b: UInt): UInt =
    ((a.asSInt +& b.asSInt) >> 1).asUInt(15, 0)
  def avg32(a: UInt, b: UInt): UInt =
    ((a.asSInt +& b.asSInt) >> 1).asUInt(31, 0)

  def packedByte(a: UInt, b: UInt): UInt = {
    val out = Wire(Vec(4, UInt(8.W)))
    for (i <- 0 until 4) {
      val x = a(8 * i + 7, 8 * i)
      val y = b(8 * i + 7, 8 * i)
      out(i) := MuxCase(
        (x +& y)(7, 0),
        Seq(
          (io.instr_type === RISCV_TYPE.psub_db) -> (x -& y)(7, 0),
          (io.instr_type === RISCV_TYPE.psadd_db) -> sat8(x.asSInt +& y.asSInt),
          (io.instr_type === RISCV_TYPE.paadd_db) -> avg8(x, y)
        )
      )
    }
    out.asUInt
  }

  def packedHalf(a: UInt, b: UInt): UInt = {
    val out = Wire(Vec(2, UInt(16.W)))
    for (i <- 0 until 2) {
      val x = a(16 * i + 15, 16 * i)
      val y = b(16 * i + 15, 16 * i)
      out(i) := MuxCase(
        (x +& y)(15, 0),
        Seq(
          (io.instr_type === RISCV_TYPE.psub_dh) -> (x -& y)(15, 0),
          (io.instr_type === RISCV_TYPE.psadd_dh) -> sat16(
            x.asSInt +& y.asSInt
          ),
          (io.instr_type === RISCV_TYPE.paadd_dh) -> avg16(x, y)
        )
      )
    }
    out.asUInt
  }

  def packedWord(a: UInt, b: UInt): UInt =
    MuxCase(
      (a +& b)(31, 0),
      Seq(
        (io.instr_type === RISCV_TYPE.psub_dw) -> (a -& b)(31, 0),
        (io.instr_type === RISCV_TYPE.psadd_dw) -> sat32(a.asSInt +& b.asSInt),
        (io.instr_type === RISCV_TYPE.paadd_dw) -> avg32(a, b)
      )
    )

  def scalarPacked(a: UInt, b: UInt): UInt =
    MuxCase(
      a,
      Seq(
        (io.instr_type === RISCV_TYPE.padd_dbs) -> {
          val out = Wire(Vec(4, UInt(8.W)))
          for (i <- 0 until 4) {
            out(i) := (a(8 * i + 7, 8 * i) +& b(7, 0))(7, 0)
          }
          out.asUInt
        },
        (io.instr_type === RISCV_TYPE.padd_dhs) -> {
          val out = Wire(Vec(2, UInt(16.W)))
          for (i <- 0 until 2) {
            out(i) := (a(16 * i + 15, 16 * i) +& b(15, 0))(15, 0)
          }
          out.asUInt
        },
        (io.instr_type === RISCV_TYPE.padd_dws) -> ((a +& b)(31, 0))
      )
    )

  def widenByte(a: UInt, b: UInt): (UInt, UInt) = {
    val sums = (0 until 4).map(i => a(8 * i + 7, 8 * i) +& b(8 * i + 7, 8 * i))
    val lanes = sums.map(sum => Cat(0.U(7.W), sum))
    (Cat(lanes(1), lanes(0)), Cat(lanes(3), lanes(2)))
  }

  def widenHalf(a: UInt, b: UInt): (UInt, UInt) = {
    val sums =
      (0 until 2).map(i => a(16 * i + 15, 16 * i) +& b(16 * i + 15, 16 * i))
    (Cat(0.U(15.W), sums(0)), Cat(0.U(15.W), sums(1)))
  }

  val imm8 = Fill(4, io.instr(27, 20))
  val imm10 = Cat(io.instr(20), io.instr(29, 21))
  val immH = Fill(2, imm10.asSInt.pad(16).asUInt)
  val immHU = Fill(2, (imm10.asSInt.pad(16).asUInt << 6)(15, 0))

  val readA = io_reg.reg_read_data1
  val readB = io_reg.reg_read_data2
  val currentResult = MuxCase(
    0.U,
    Seq(
      (io.instr_type === RISCV_TYPE.pli_db) -> imm8,
      (io.instr_type === RISCV_TYPE.pli_dh) -> immH,
      (io.instr_type === RISCV_TYPE.plui_dh) -> immHU,
      (io.instr_type === RISCV_TYPE.padd_db) -> packedByte(readA, readB),
      (io.instr_type === RISCV_TYPE.psub_db) -> packedByte(readA, readB),
      (io.instr_type === RISCV_TYPE.psadd_db) -> packedByte(readA, readB),
      (io.instr_type === RISCV_TYPE.paadd_db) -> packedByte(readA, readB),
      (io.instr_type === RISCV_TYPE.padd_dh) -> packedHalf(readA, readB),
      (io.instr_type === RISCV_TYPE.psub_dh) -> packedHalf(readA, readB),
      (io.instr_type === RISCV_TYPE.psadd_dh) -> packedHalf(readA, readB),
      (io.instr_type === RISCV_TYPE.paadd_dh) -> packedHalf(readA, readB),
      (io.instr_type === RISCV_TYPE.padd_dw) -> packedWord(readA, readB),
      (io.instr_type === RISCV_TYPE.psub_dw) -> packedWord(readA, readB),
      (io.instr_type === RISCV_TYPE.psadd_dw) -> packedWord(readA, readB),
      (io.instr_type === RISCV_TYPE.paadd_dw) -> packedWord(readA, readB),
      isScalar -> scalarPacked(readA, readB)
    )
  )

  val widen = Mux(
    io.instr_type === RISCV_TYPE.pwadd_b || io.instr_type === RISCV_TYPE.pwadda_b,
    Cat(widenByte(readA, readB)._2, widenByte(readA, readB)._1),
    Cat(widenHalf(readA, readB)._2, widenHalf(readA, readB)._1)
  )

  io_reg.reg_rs1 := Mux(isImm, 0.U, rs1)
  io_reg.reg_rs2 := Mux(isImm, 0.U, rs2)
  io_reg.reg_rd := rd
  io_reg.reg_write_en := io.valid && !isAcc
  io_reg.reg_write_data := Mux(isWiden, widen(31, 0), currentResult)
  io.stall := Mux(io.valid, STALL_REASON.EXECUTION_UNIT, STALL_REASON.NO_STALL)

  when(io.valid && phase === 0.U) {
    when(isWiden) {
      lowSaved := widen(31, 0)
      highSaved := widen(63, 32)
    }
    phase := 1.U
  }

  when(phase === 1.U) {
    io_reg.reg_rs1 := Mux(isImm, 0.U, rs1High)
    io_reg.reg_rs2 := Mux(isImm, 0.U, Mux(isScalar, rs2, rs2High))
    io_reg.reg_rd := rdHigh
    io_reg.reg_write_en := !isAcc
    io_reg.reg_write_data := Mux(isWiden, highSaved, currentResult)
    io.stall := Mux(isAcc, STALL_REASON.EXECUTION_UNIT, STALL_REASON.NO_STALL)
    io_pc.pc_we := !isAcc
    when(isAcc) {
      io_reg.reg_rs1 := rd
      io_reg.reg_rs2 := rdHigh
      io_reg.reg_rd := rd
      io_reg.reg_write_en := true.B
      io_reg.reg_write_data := lowSaved + readA
      highSaved := highSaved + readB
      phase := 2.U
    }.otherwise {
      phase := 0.U
    }
  }

  when(phase === 2.U) {
    io_reg.reg_rs1 := 0.U
    io_reg.reg_rs2 := 0.U
    io_reg.reg_rd := rdHigh
    io_reg.reg_write_en := true.B
    io_reg.reg_write_data := highSaved
    io.stall := STALL_REASON.NO_STALL
    io_pc.pc_we := true.B
    phase := 0.U
  }

  when(~io_reset.rst_n) {
    phase := 0.U
    lowSaved := 0.U
    highSaved := 0.U
    io_pc.pc_we := false.B
    io_reg.reg_write_en := false.B
  }
}
