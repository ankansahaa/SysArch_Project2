package RISCV.implementation.RV32P

import chisel3._
import chisel3.util._
import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._

class MergeUnit extends AbstractExecutionUnit {
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

  val supported =
    Seq(RISCV_TYPE.mvm, RISCV_TYPE.mvmn, RISCV_TYPE.merge, RISCV_TYPE.pack)
  io.valid := supported.map(_ === io.instr_type).reduce(_ || _)

  val cycle = RegInit(0.U(2.W))
  val savedRs1 = Reg(UInt(32.W))
  val savedRs2 = Reg(UInt(32.W))
  val busy = RegInit(false.B)

  io.stall := Mux(busy, STALL_REASON.EXECUTION_UNIT, STALL_REASON.NO_STALL)

  val rs1_addr = io.instr(19, 15)
  val rs2_addr = io.instr(24, 20)
  val rd_addr = io.instr(11, 7)

  val needsRd = io.instr_type === RISCV_TYPE.mvm ||
    io.instr_type === RISCV_TYPE.mvmn ||
    io.instr_type === RISCV_TYPE.merge

  io_reg.reg_rs1 := rs1_addr
  io_reg.reg_rs2 := rs2_addr
  io_reg.reg_rd := rd_addr
  io_reg.reg_write_en := io.valid && !needsRd
  io_reg.reg_write_data := Mux(
    io.instr_type === RISCV_TYPE.pack,
    Cat(io_reg.reg_read_data2(15, 0), io_reg.reg_read_data1(15, 0)),
    0.U
  )

  when(io.valid && !busy) {
    when(needsRd) {
      busy := true.B
      cycle := 1.U
      savedRs1 := io_reg.reg_read_data1
      savedRs2 := io_reg.reg_read_data2
    }
  }

  when(busy && cycle === 1.U) {
    io_reg.reg_rs1 := rd_addr
    io_reg.reg_rs2 := 0.U
    io_reg.reg_write_en := true.B
    val oldRd = io_reg.reg_read_data1
    io_reg.reg_write_data := MuxCase(
      0.U,
      Seq(
        (io.instr_type === RISCV_TYPE.mvm) -> ((~savedRs2 & oldRd) | (savedRs2 & savedRs1)),
        (io.instr_type === RISCV_TYPE.mvmn) -> ((~savedRs2 & savedRs1) | (savedRs2 & oldRd)),
        (io.instr_type === RISCV_TYPE.merge) -> ((~oldRd & savedRs1) | (oldRd & savedRs2))
      )
    )
    cycle := 0.U
    busy := false.B
  }

  val finishing = busy && cycle === 1.U
  io.stall := Mux(
    (busy && !finishing) || (io.valid && needsRd && !busy),
    STALL_REASON.EXECUTION_UNIT,
    STALL_REASON.NO_STALL
  )
  io_pc.pc_we := io.valid && io.stall === STALL_REASON.NO_STALL

  when(~io_reset.rst_n) {
    busy := false.B
    cycle := 0.U
    savedRs1 := 0.U
    savedRs2 := 0.U
    io_reg.reg_write_en := false.B
    io_pc.pc_we := false.B
  }
}
