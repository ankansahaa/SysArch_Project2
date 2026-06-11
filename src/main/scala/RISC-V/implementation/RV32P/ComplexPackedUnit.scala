package RISCV.implementation.RV32P

import chisel3._
import chisel3.util._
import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._

class ComplexPackedUnit extends AbstractExecutionUnit {
  io.misa := "b01__0000__0_00000_00000_00000_10000_00000".U

  // Default outputs – must be assigned unconditionally
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

  val cycle = RegInit(0.U(3.W))
  val result = Reg(UInt(32.W))
  val busy = RegInit(false.B)

  io.stall := Mux(busy, STALL_REASON.EXECUTION_UNIT, STALL_REASON.NO_STALL)

  val rs1_addr = io.instr(19, 15)
  val rs2_addr = io.instr(24, 20)
  val rd_addr = io.instr(11, 7)

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
    when(cycle === 1.U) {
      val rs1 = io_reg.reg_read_data1
      val rs2 = io_reg.reg_read_data2
      def clip(value: UInt, shift: UInt, bits: Int): UInt = {
        val max = (1 << (bits - 1)) - 1
        val min = -(1 << (bits - 1))
        val shifted = value.asSInt >> shift
        Mux(
          shifted > max.S,
          max.U,
          Mux(shifted < min.S, (BigInt(1) << bits).U, shifted.asUInt)
        )
      }
      result := MuxCase(
        0.U,
        Seq(
          (io.instr_type === RISCV_TYPE.pssha_hs) -> ((rs1.asSInt >> rs2(
            4,
            0
          )).asUInt),
          (io.instr_type === RISCV_TYPE.psshar_hs) -> ((rs1.asSInt >> rs2(
            4,
            0
          )).asUInt),
          (io.instr_type === RISCV_TYPE.psshl_hs) -> ((rs1.asSInt << rs2(
            4,
            0
          )).asUInt),
          (io.instr_type === RISCV_TYPE.psshlr_hs) -> ((rs1.asSInt << rs2(
            4,
            0
          )).asUInt),
          (io.instr_type === RISCV_TYPE.pnclipi_b) -> clip(rs1, rs2(4, 0), 8),
          (io.instr_type === RISCV_TYPE.pnclipri_b) -> clip(rs1, rs2(4, 0), 8),
          (io.instr_type === RISCV_TYPE.pnclipi_h) -> clip(rs1, rs2(4, 0), 16),
          (io.instr_type === RISCV_TYPE.pnclipri_h) -> clip(rs1, rs2(4, 0), 16),
          (io.instr_type === RISCV_TYPE.pm2add_h) -> (rs1 +& (rs2 * 2.U))(
            31,
            0
          ),
          (io.instr_type === RISCV_TYPE.pm2addu_h) -> (rs1 +& (rs2 * 2.U))(
            31,
            0
          ),
          (io.instr_type === RISCV_TYPE.pm2addsu_h) -> (rs1 +& (rs2 * 2.U))(
            31,
            0
          ),
          (io.instr_type === RISCV_TYPE.pm2add_hx) -> (rs1 +& (rs2 * 2.U))(
            31,
            0
          ),
          (io.instr_type === RISCV_TYPE.pm2adda_h) -> (rs1 +& (rs2 * 2.U))(
            31,
            0
          ),
          (io.instr_type === RISCV_TYPE.pm2addau_h) -> (rs1 +& (rs2 * 2.U))(
            31,
            0
          ),
          (io.instr_type === RISCV_TYPE.pm2addasu_h) -> (rs1 +& (rs2 * 2.U))(
            31,
            0
          ),
          (io.instr_type === RISCV_TYPE.pm2adda_hx) -> (rs1 +& (rs2 * 2.U))(
            31,
            0
          ),
          (io.instr_type === RISCV_TYPE.pm2sadd_h) -> (rs1 -& (rs2 * 2.U))(
            31,
            0
          ),
          (io.instr_type === RISCV_TYPE.pm2sadd_hx) -> (rs1 -& (rs2 * 2.U))(
            31,
            0
          )
        )
      )
      cycle := 0.U
      busy := false.B
    }
  }
}
