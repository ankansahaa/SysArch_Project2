package RISCV.implementation.generic.caches

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractCache
import RISCV.model.CACHE_STATE

class FIFOCache(capacity: Int) extends AbstractCache {
  val state = RegInit(CACHE_STATE.IDLE)
  val tags = Reg(Vec(capacity, UInt(30.W)))
  val data = Reg(Vec(capacity, UInt(32.W)))
  val valid = RegInit(VecInit(Seq.fill(capacity)(false.B)))
  val fifoPtr = RegInit(0.U(log2Ceil(capacity).W))

  val reqAddr = Reg(UInt(32.W))
  val reqBe = Reg(UInt(4.W))
  val reqWe = Reg(Bool())
  val reqWdata = Reg(UInt(32.W))
  val reqCoreGnt = Reg(Bool())

  core_io.data_gnt := false.B
  core_io.data_rdata := 0.U // default

  mem_io.data_req := false.B
  mem_io.data_addr := 0.U
  mem_io.data_be := 0.U
  mem_io.data_we := false.B
  mem_io.data_wdata := 0.U

  switch(state) {
    is(CACHE_STATE.IDLE) {
      when(core_io.data_req) {
        val tag = core_io.data_addr >> 2
        val hitIndex = (0 until capacity).map(i => valid(i) && tags(i) === tag)
        val hitVec = Cat(hitIndex.reverse)
        when(hitVec.orR) {
          val idx = OHToUInt(hitVec)
          core_io.data_gnt := true.B
          core_io.data_rdata := data(idx)
          when(core_io.data_we) {
            val mask = Cat(
              Fill(8, core_io.data_be(3)),
              Fill(8, core_io.data_be(2)),
              Fill(8, core_io.data_be(1)),
              Fill(8, core_io.data_be(0))
            )
            data(idx) := (mask & core_io.data_wdata) | (~mask & data(idx))
          }
        }.otherwise {
          reqAddr := core_io.data_addr
          reqBe := core_io.data_be
          reqWe := core_io.data_we
          reqWdata := core_io.data_wdata
          reqCoreGnt := false.B
          state := CACHE_STATE.READ
          mem_io.data_req := true.B
          mem_io.data_addr := core_io.data_addr
          mem_io.data_be := "b1111".U
          mem_io.data_we := false.B
        }
      }
    }

    is(CACHE_STATE.READ) {
      when(mem_io.data_gnt) {
        val newIdx = fifoPtr
        tags(newIdx) := reqAddr >> 2
        data(newIdx) := mem_io.data_rdata
        valid(newIdx) := true.B
        fifoPtr := fifoPtr + 1.U
        when(reqWe) {
          val mask = Cat(
            Fill(8, reqBe(3)),
            Fill(8, reqBe(2)),
            Fill(8, reqBe(1)),
            Fill(8, reqBe(0))
          )
          data(newIdx) := (mask & reqWdata) | (~mask & mem_io.data_rdata)
        }
        core_io.data_rdata := mem_io.data_rdata
        core_io.data_gnt := true.B
        state := CACHE_STATE.IDLE
      }
    }

    is(CACHE_STATE.WRITE) {
      state := CACHE_STATE.IDLE
    }
  }
}
