package RISCV.implementation.RV32I

import chisel3._
import chisel3.util._

import RISCV.interfaces.RV32I.AbstractControlUnit
import RISCV.model._

class ControlUnit extends AbstractControlUnit {
  val stalled = WireInit(STALL_REASON.NO_STALL)
  val was_stalled = RegInit(STALL_REASON.NO_STALL)
  when(~io_reset.rst_n) {
    was_stalled := STALL_REASON.NO_STALL
  }.otherwise {
    was_stalled := stalled
  }

  io_ctrl.stall := stalled
  io_ctrl.reg_we := false.B

  io_ctrl.reg_write_sel := REG_WRITE_SEL.ALU_OUT
  io_ctrl.alu_control := ALU_CONTROL.ADD
  io_ctrl.alu_op_1_sel := ALU_OP_1_SEL.RS1
  io_ctrl.alu_op_2_sel := ALU_OP_2_SEL.RS2

  io_ctrl.data_req := false.B
  io_ctrl.data_we := false.B
  io_ctrl.data_be := 0.U

  io_ctrl.next_pc_select := NEXT_PC_SELECT.PC_PLUS_4

  when(was_stalled === STALL_REASON.EXECUTION_UNIT) {
    when(io_ctrl.data_gnt) {
      stalled := STALL_REASON.NO_STALL
      when(RISCV_TYPE.getOP(io_ctrl.instr_type) === RISCV_OP.LOAD) {
        io_ctrl.reg_we := true.B
        switch(RISCV_TYPE.getFunct3(io_ctrl.instr_type)) {
          is(RISCV_FUNCT3.F000, RISCV_FUNCT3.F001) {
            io_ctrl.reg_write_sel := REG_WRITE_SEL.MEM_OUT_SIGN_EXTENDED
          }
          is(RISCV_FUNCT3.F010, RISCV_FUNCT3.F100, RISCV_FUNCT3.F101) {
            io_ctrl.reg_write_sel := REG_WRITE_SEL.MEM_OUT_ZERO_EXTENDED
          }
        }
      }
    }
  }.otherwise {
    switch(RISCV_TYPE.getOP(io_ctrl.instr_type)) {
      is(RISCV_OP.OP_IMM) {
        stalled := STALL_REASON.NO_STALL
        io_ctrl.reg_we := true.B
        io_ctrl.reg_write_sel := REG_WRITE_SEL.ALU_OUT
        io_ctrl.alu_control := ALU_CONTROL(
          RISCV_TYPE.getFunct7(io_ctrl.instr_type).asUInt(5) ##
            RISCV_TYPE.getFunct3(io_ctrl.instr_type).asUInt
        )
        io_ctrl.alu_op_1_sel := ALU_OP_1_SEL.RS1
        io_ctrl.alu_op_2_sel := ALU_OP_2_SEL.IMM
      }
      is(RISCV_OP.OP) {
        stalled := STALL_REASON.NO_STALL
        io_ctrl.reg_we := true.B
        io_ctrl.reg_write_sel := REG_WRITE_SEL.ALU_OUT
        io_ctrl.alu_control := ALU_CONTROL(
          RISCV_TYPE.getFunct7(io_ctrl.instr_type).asUInt(5) ##
            RISCV_TYPE.getFunct3(io_ctrl.instr_type).asUInt
        )
        io_ctrl.alu_op_1_sel := ALU_OP_1_SEL.RS1
        io_ctrl.alu_op_2_sel := ALU_OP_2_SEL.RS2
      }
      is(RISCV_OP.LUI) {
        stalled := STALL_REASON.NO_STALL
        io_ctrl.reg_we := true.B
        io_ctrl.reg_write_sel := REG_WRITE_SEL.IMM
      }
      is(RISCV_OP.AUIPC) {
        stalled := STALL_REASON.NO_STALL
        io_ctrl.reg_we := true.B
        io_ctrl.reg_write_sel := REG_WRITE_SEL.ALU_OUT
        io_ctrl.alu_control := ALU_CONTROL.ADD
        io_ctrl.alu_op_1_sel := ALU_OP_1_SEL.PC
        io_ctrl.alu_op_2_sel := ALU_OP_2_SEL.IMM
      }
      is(RISCV_OP.BRANCH) {
        stalled := STALL_REASON.NO_STALL
        io_ctrl.reg_we := false.B
        io_ctrl.reg_write_sel := REG_WRITE_SEL.ALU_OUT
        io_ctrl.alu_control := ALU_CONTROL(
          (~RISCV_TYPE.getFunct3(io_ctrl.instr_type).asUInt(2)) ##
            Fill(1, 0.U) ##
            RISCV_TYPE.getFunct3(io_ctrl.instr_type).asUInt(2, 1)
        )
        io_ctrl.alu_op_1_sel := ALU_OP_1_SEL.RS1
        io_ctrl.alu_op_2_sel := ALU_OP_2_SEL.RS2
        io_ctrl.next_pc_select := NEXT_PC_SELECT.BRANCH
      }
      is(RISCV_OP.STORE) {
        stalled := STALL_REASON.EXECUTION_UNIT
        io_ctrl.alu_control := ALU_CONTROL.ADD
        io_ctrl.alu_op_1_sel := ALU_OP_1_SEL.RS1
        io_ctrl.alu_op_2_sel := ALU_OP_2_SEL.IMM
        io_ctrl.reg_we := false.B
        io_ctrl.data_req := true.B
        io_ctrl.data_we := true.B
        val funct3 = RISCV_TYPE.getFunct3(io_ctrl.instr_type).asUInt
        io_ctrl.data_be := Fill(2, funct3(1)) ## funct3(1, 0).orR ## 1.U(1.W)
      }
      is(RISCV_OP.JAL) {
        stalled := STALL_REASON.NO_STALL
        io_ctrl.reg_we := true.B
        io_ctrl.reg_write_sel := REG_WRITE_SEL.PC_PLUS_4
        io_ctrl.next_pc_select := NEXT_PC_SELECT.IMM
      }
      is(RISCV_OP.JALR) {
        stalled := STALL_REASON.NO_STALL
        io_ctrl.reg_we := true.B
        io_ctrl.reg_write_sel := REG_WRITE_SEL.PC_PLUS_4
        io_ctrl.alu_control := ALU_CONTROL.ADD
        io_ctrl.alu_op_1_sel := ALU_OP_1_SEL.RS1
        io_ctrl.alu_op_2_sel := ALU_OP_2_SEL.IMM
        io_ctrl.next_pc_select := NEXT_PC_SELECT.ALU_OUT_ALIGNED
      }
      is(RISCV_OP.LOAD) {
        stalled := STALL_REASON.EXECUTION_UNIT
        io_ctrl.alu_control := ALU_CONTROL.ADD
        io_ctrl.alu_op_1_sel := ALU_OP_1_SEL.RS1
        io_ctrl.alu_op_2_sel := ALU_OP_2_SEL.IMM
        io_ctrl.reg_we := false.B
        io_ctrl.data_req := true.B
        io_ctrl.data_we := false.B
        val funct3 = RISCV_TYPE.getFunct3(io_ctrl.instr_type)
        io_ctrl.data_be := MuxCase(
          0.U(4.W),
          Seq(
            (funct3 === RISCV_FUNCT3.F000) -> 1.U(4.W),
            (funct3 === RISCV_FUNCT3.F100) -> 1.U(4.W),
            (funct3 === RISCV_FUNCT3.F001) -> 3.U(4.W),
            (funct3 === RISCV_FUNCT3.F101) -> 3.U(4.W),
            (funct3 === RISCV_FUNCT3.F010) -> 15.U(4.W)
          )
        )
        switch(funct3) {
          is(RISCV_FUNCT3.F010) {
            io_ctrl.reg_write_sel := REG_WRITE_SEL.MEM_OUT_ZERO_EXTENDED
          }
          is(RISCV_FUNCT3.F000) {
            io_ctrl.reg_write_sel := REG_WRITE_SEL.MEM_OUT_SIGN_EXTENDED
          }
          is(RISCV_FUNCT3.F100) {
            io_ctrl.reg_write_sel := REG_WRITE_SEL.MEM_OUT_ZERO_EXTENDED
          }
          is(RISCV_FUNCT3.F001) {
            io_ctrl.reg_write_sel := REG_WRITE_SEL.MEM_OUT_SIGN_EXTENDED
          }
          is(RISCV_FUNCT3.F101) {
            io_ctrl.reg_write_sel := REG_WRITE_SEL.MEM_OUT_ZERO_EXTENDED
          }
        }
      }
    }
  }
}
