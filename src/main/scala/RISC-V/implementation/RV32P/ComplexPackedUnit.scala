package RISCV.implementation.RV32P

import chisel3._
import chisel3.util._
import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._

class ComplexPackedUnit extends AbstractExecutionUnit {
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
    RISCV_TYPE.pssha_hs,
    RISCV_TYPE.psshar_hs,
    RISCV_TYPE.psshl_hs,
    RISCV_TYPE.psshlr_hs,
    RISCV_TYPE.pnclipi_b,
    RISCV_TYPE.pnclipi_h,
    RISCV_TYPE.pnclipri_b,
    RISCV_TYPE.pnclipri_h,
    RISCV_TYPE.pm2add_h,
    RISCV_TYPE.pm2addu_h,
    RISCV_TYPE.pm2addsu_h,
    RISCV_TYPE.pm2add_hx,
    RISCV_TYPE.pm2adda_h,
    RISCV_TYPE.pm2addau_h,
    RISCV_TYPE.pm2addasu_h,
    RISCV_TYPE.pm2adda_hx,
    RISCV_TYPE.pm2sadd_h,
    RISCV_TYPE.pm2sadd_hx
  )
  io.valid := supported.map(_ === io.instr_type).reduce(_ || _)

  val phase = RegInit(0.U(1.W))
  val saved = RegInit(0.U(32.W))

  val rd = io.instr(11, 7)
  val rs1 = io.instr(19, 15)
  val rs2 = io.instr(24, 20)
  val pairRs1 = Cat(rs1(4, 1), 0.U(1.W))
  // PNCLIPI.H uses a 5-bit immediate in instr[24:20]; for PNCLIPI.B
  // instr[24] is a fixed 1 and the immediate is only instr[23:20].
  val shamtH = io.instr(24, 20)
  val shamtB = io.instr(23, 20)
  val isClip = io.instr_type === RISCV_TYPE.pnclipi_b ||
    io.instr_type === RISCV_TYPE.pnclipi_h ||
    io.instr_type === RISCV_TYPE.pnclipri_b ||
    io.instr_type === RISCV_TYPE.pnclipri_h
  val isAcc = io.instr_type === RISCV_TYPE.pm2adda_h ||
    io.instr_type === RISCV_TYPE.pm2addau_h ||
    io.instr_type === RISCV_TYPE.pm2addasu_h ||
    io.instr_type === RISCV_TYPE.pm2adda_hx

  io_reg.reg_rs1 := Mux(isClip, pairRs1, rs1)
  io_reg.reg_rs2 := Mux(isClip, pairRs1 + 1.U, rs2)
  io_reg.reg_rd := rd
  io_reg.reg_write_en := io.valid && !isAcc
  io.stall := Mux(
    io.valid && isAcc && phase === 0.U,
    STALL_REASON.EXECUTION_UNIT,
    STALL_REASON.NO_STALL
  )
  io_pc.pc_we := io.valid && io.stall === STALL_REASON.NO_STALL

  def clipS8(x: SInt): UInt =
    Mux(x > 127.S, "h7f".U, Mux(x < -128.S, "h80".U, x.asUInt(7, 0)))
  def clipS16(x: SInt): UInt =
    Mux(x > 32767.S, "h7fff".U, Mux(x < -32768.S, "h8000".U, x.asUInt(15, 0)))
  def clipU16(x: UInt): UInt = Mux(x > "hffff".U, "hffff".U, x(15, 0))

  def roundRightSigned(x: SInt, amount: UInt): SInt =
    (((x << 1).asSInt >> amount) + 1.S) >> 1
  def roundRightUnsigned(x: UInt, amount: UInt): UInt =
    ((((x << 1) >> amount) + 1.U) >> 1)

  def shiftSignedHalf(round: Boolean): UInt = {
    val out = Wire(Vec(2, UInt(16.W)))
    val sa = io_reg.reg_read_data2(7, 0).asSInt
    for (i <- 0 until 2) {
      val h = io_reg.reg_read_data1(16 * i + 15, 16 * i).asSInt
      val neg = -sa
      val rightAmt = Mux(neg > 16.S, 16.U, neg.asUInt(4, 0))
      val right = Mux(round.B, roundRightSigned(h, rightAmt), h >> rightAmt)
      val left = clipS16(h << sa.asUInt(4, 0))
      out(i) := Mux(
        sa < 0.S,
        Mux(
          neg >= 16.S && !round.B,
          Mux(h < 0.S, "hffff".U, 0.U),
          right.asUInt(15, 0)
        ),
        left
      )
    }
    out.asUInt
  }

  def shiftUnsignedHalf(round: Boolean): UInt = {
    val out = Wire(Vec(2, UInt(16.W)))
    val sa = io_reg.reg_read_data2(7, 0).asSInt
    for (i <- 0 until 2) {
      val h = io_reg.reg_read_data1(16 * i + 15, 16 * i)
      val neg = -sa
      val rightAmt = Mux(neg > 16.S, 16.U, neg.asUInt(4, 0))
      val right = Mux(round.B, roundRightUnsigned(h, rightAmt), h >> rightAmt)
      val left = clipU16(h << sa.asUInt(4, 0))
      out(i) := Mux(
        sa < 0.S,
        Mux(neg >= 16.S && !round.B, 0.U, right(15, 0)),
        left
      )
    }
    out.asUInt
  }

  def clipBytes(round: Boolean): UInt = {
    val out = Wire(Vec(4, UInt(8.W)))
    val words = VecInit(io_reg.reg_read_data1, io_reg.reg_read_data2)
    for (i <- 0 until 4) {
      val h = words(i / 2)(16 * (i % 2) + 15, 16 * (i % 2)).asSInt
      val shifted = Mux(round.B, roundRightSigned(h, shamtB), h >> shamtB)
      out(i) := clipS8(shifted)
    }
    out.asUInt
  }

  def clipHalfs(round: Boolean): UInt = {
    val out = Wire(Vec(2, UInt(16.W)))
    val words = VecInit(io_reg.reg_read_data1, io_reg.reg_read_data2)
    for (i <- 0 until 2) {
      val w = words(i).asSInt
      val shifted = Mux(round.B, roundRightSigned(w, shamtH), w >> shamtH)
      out(i) := clipS16(shifted)
    }
    out.asUInt
  }

  def halfMul(a: UInt, signedA: Bool, b: UInt, signedB: Bool): SInt = {
    val aa = Mux(signedA, a.asSInt.pad(32), a.zext)
    val bb = Mux(signedB, b.asSInt.pad(32), b.zext)
    aa * bb
  }

  def m2sum(cross: Bool, signedA: Bool, signedB: Bool): SInt = {
    val a0 = io_reg.reg_read_data1(15, 0)
    val a1 = io_reg.reg_read_data1(31, 16)
    val b0 = io_reg.reg_read_data2(15, 0)
    val b1 = io_reg.reg_read_data2(31, 16)
    val p0 = Mux(
      cross,
      halfMul(a0, signedA, b1, signedB),
      halfMul(a0, signedA, b0, signedB)
    )
    val p1 = Mux(
      cross,
      halfMul(a1, signedA, b0, signedB),
      halfMul(a1, signedA, b1, signedB)
    )
    p0 +& p1
  }

  def m2add(cross: Bool, signedA: Bool, signedB: Bool): UInt = {
    m2sum(cross, signedA, signedB).asUInt(31, 0)
  }

  def m2sadd(cross: Bool): UInt = {
    val sum = m2sum(cross, true.B, true.B)
    Mux(
      sum > 2147483647.S,
      "h7fffffff".U,
      Mux(sum < -2147483648L.S, "h80000000".U, sum.asUInt)
    )
  }

  val addSigned = io.instr_type === RISCV_TYPE.pm2add_h ||
    io.instr_type === RISCV_TYPE.pm2add_hx ||
    io.instr_type === RISCV_TYPE.pm2adda_h ||
    io.instr_type === RISCV_TYPE.pm2adda_hx
  val addUnsigned = io.instr_type === RISCV_TYPE.pm2addu_h ||
    io.instr_type === RISCV_TYPE.pm2addau_h
  val addMixed = io.instr_type === RISCV_TYPE.pm2addsu_h ||
    io.instr_type === RISCV_TYPE.pm2addasu_h
  val addCross = io.instr_type === RISCV_TYPE.pm2add_hx ||
    io.instr_type === RISCV_TYPE.pm2adda_hx

  val result = MuxCase(
    0.U,
    Seq(
      (io.instr_type === RISCV_TYPE.pssha_hs) -> shiftSignedHalf(false),
      (io.instr_type === RISCV_TYPE.psshar_hs) -> shiftSignedHalf(true),
      (io.instr_type === RISCV_TYPE.psshl_hs) -> shiftUnsignedHalf(false),
      (io.instr_type === RISCV_TYPE.psshlr_hs) -> shiftUnsignedHalf(true),
      (io.instr_type === RISCV_TYPE.pnclipi_b) -> clipBytes(false),
      (io.instr_type === RISCV_TYPE.pnclipri_b) -> clipBytes(true),
      (io.instr_type === RISCV_TYPE.pnclipi_h) -> clipHalfs(false),
      (io.instr_type === RISCV_TYPE.pnclipri_h) -> clipHalfs(true),
      (addSigned || addMixed || addUnsigned || isAcc) -> m2add(
        addCross,
        addSigned || addMixed,
        addSigned
      ),
      (io.instr_type === RISCV_TYPE.pm2sadd_h) -> m2sadd(false.B),
      (io.instr_type === RISCV_TYPE.pm2sadd_hx) -> m2sadd(true.B)
    )
  )

  io_reg.reg_write_data := result

  when(io.valid && isAcc && phase === 0.U) {
    saved := result
    phase := 1.U
  }

  when(phase === 1.U) {
    io_reg.reg_rs1 := rd
    io_reg.reg_rs2 := 0.U
    io_reg.reg_rd := rd
    io_reg.reg_write_en := true.B
    io_reg.reg_write_data := io_reg.reg_read_data1 + saved
    io.stall := STALL_REASON.NO_STALL
    io_pc.pc_we := true.B
    phase := 0.U
  }

  when(~io_reset.rst_n) {
    phase := 0.U
    saved := 0.U
    io_pc.pc_we := false.B
    io_reg.reg_write_en := false.B
  }
}
