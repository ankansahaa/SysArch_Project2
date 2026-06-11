package RISCV.implementation.RV32P

import chisel3._
import chisel3.util._
import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._

class MergeUnit extends AbstractExecutionUnit {
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

  val supported =
    Seq(RISCV_TYPE.mvm, RISCV_TYPE.mvmn, RISCV_TYPE.merge, RISCV_TYPE.pack)
  io.valid := supported.map(_ === io.instr_type).reduce(_ || _)

  val cycle = RegInit(0.U(2.W))
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

  when(busy && cycle === 1.U) {
    val rs1 = io_reg.reg_read_data1
    val rs2 = io_reg.reg_read_data2
    result := MuxCase(
      0.U,
      Seq(
        (io.instr_type === RISCV_TYPE.mvm) -> (rs1 & rs2),
        (io.instr_type === RISCV_TYPE.mvmn) -> (rs1 | rs2),
        (io.instr_type === RISCV_TYPE.merge) -> ((rs1 & 0xffff0000.U) | (rs2 & 0xffff.U)),
        (io.instr_type === RISCV_TYPE.pack) -> ((rs1(15, 0) ## rs2(15, 0)))
      )
    )
    cycle := 0.U; busy := false.B
  }
}
