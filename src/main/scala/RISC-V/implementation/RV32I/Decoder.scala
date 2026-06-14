package RISCV.implementation.RV32I

import chisel3._
import chisel3.util._

import RISCV.model._
import RISCV.interfaces.RV32I.AbstractDecoder

class Decoder extends AbstractDecoder {
  val opcode = RISCV_OP(io_decoder.instr(6, 0))
  val funct3 = RISCV_FUNCT3(io_decoder.instr(14, 12))
  val funct7 = RISCV_FUNCT7(io_decoder.instr(31, 25))
  val funct12 = RISCV_FUNCT12(io_decoder.instr(31, 20))

  val RS1 = io_decoder.instr(19, 15)
  val RS2 = io_decoder.instr(24, 20)
  val RD = io_decoder.instr(11, 7)

  // Default values
  io_decoder.rs1 := 0.U
  io_decoder.rs2 := 0.U
  io_decoder.rd := 0.U
  io_decoder.imm := 0.U

  switch(opcode) {
    is(RISCV_OP.OP) {
      io_decoder.rs1 := RS1
      io_decoder.rs2 := RS2
      io_decoder.rd := RD
      io_decoder.imm := 0.U
    }
    is(RISCV_OP.OP_IMM) {
      io_decoder.rs1 := RS1
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      io_decoder.imm := Fill(20, io_decoder.instr(31)) ## io_decoder.instr(
        31,
        20
      )
    }
    is(RISCV_OP.LUI) {
      io_decoder.rs1 := 0.U
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      io_decoder.imm := io_decoder.instr(31, 12) ## Fill(12, 0.U)
    }
    is(RISCV_OP.AUIPC) {
      io_decoder.rs1 := 0.U
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      io_decoder.imm := io_decoder.instr(31, 12) ## Fill(12, 0.U)
    }
    is(RISCV_OP.BRANCH) {
      io_decoder.rs1 := RS1
      io_decoder.rs2 := RS2
      io_decoder.rd := 0.U
      io_decoder.imm := Fill(19, io_decoder.instr(31)) ## io_decoder.instr(
        31
      ) ##
        io_decoder.instr(7) ## io_decoder.instr(30, 25) ##
        io_decoder.instr(11, 8) ## 0.U(1.W)
    }
    is(RISCV_OP.STORE) {
      io_decoder.rs1 := RS1
      io_decoder.rs2 := RS2
      io_decoder.rd := 0.U
      io_decoder.imm := Fill(20, io_decoder.instr(31)) ## io_decoder.instr(
        31,
        25
      ) ##
        io_decoder.instr(11, 7)
    }
    // ========== NEW for Task 2 ==========
    is(RISCV_OP.JAL) {
      io_decoder.rs1 := 0.U
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      val imm20 = io_decoder.instr(31)
      val imm10_1 = io_decoder.instr(30, 21)
      val imm11 = io_decoder.instr(20)
      val imm19_12 = io_decoder.instr(19, 12)
      io_decoder.imm := Cat(
        Fill(11, imm20),
        imm20,
        imm19_12,
        imm11,
        imm10_1,
        0.U(1.W)
      )
    }
    is(RISCV_OP.JALR) {
      io_decoder.rs1 := RS1
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      io_decoder.imm := Fill(20, io_decoder.instr(31)) ## io_decoder.instr(
        31,
        20
      )
    }
    // ========== NEW for Task 3 ==========
    is(RISCV_OP.LOAD) {
      io_decoder.rs1 := RS1
      io_decoder.rs2 := 0.U
      io_decoder.rd := RD
      io_decoder.imm := Fill(20, io_decoder.instr(31)) ## io_decoder.instr(
        31,
        20
      )
    }
  }
}
