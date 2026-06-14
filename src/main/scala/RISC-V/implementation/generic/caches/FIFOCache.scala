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
  val ptrWidth = if (capacity > 1) log2Ceil(capacity) else 1
  val fifoPtr = RegInit(0.U(ptrWidth.W))

  // Latched request parameters while a memory access is in flight
  val reqAddr = Reg(UInt(32.W))
  val reqBe = Reg(UInt(4.W))
  val reqWdata = Reg(UInt(32.W))

  // A hit is answered in the cycle after the request arrived
  val hitPending = RegInit(false.B)
  val hitData = Reg(UInt(32.W))

  def expandBe(be: UInt): UInt =
    Cat(Fill(8, be(3)), Fill(8, be(2)), Fill(8, be(1)), Fill(8, be(0)))

  def advancePtr(): Unit = {
    fifoPtr := Mux(fifoPtr === (capacity - 1).U, 0.U, fifoPtr + 1.U)
  }

  def insert(tag: UInt, word: UInt): Unit = {
    tags(fifoPtr) := tag
    data(fifoPtr) := word
    valid(fifoPtr) := true.B
    advancePtr()
  }

  def hitVecFor(addr: UInt): Vec[Bool] =
    VecInit((0 until capacity).map(i => valid(i) && tags(i) === addr(31, 2)))

  core_io.data_gnt := false.B
  core_io.data_rdata := 0.U

  mem_io.data_req := false.B
  mem_io.data_addr := 0.U
  mem_io.data_be := 0.U
  mem_io.data_we := false.B
  mem_io.data_wdata := 0.U

  hitPending := false.B

  when(hitPending) {
    core_io.data_gnt := true.B
    core_io.data_rdata := hitData
  }

  switch(state) {
    is(CACHE_STATE.IDLE) {
      when(core_io.data_req && !hitPending) {
        val hits = hitVecFor(core_io.data_addr)
        val hit = hits.asUInt.orR
        val hitIdx = PriorityEncoder(hits)

        reqAddr := core_io.data_addr
        reqBe := core_io.data_be
        reqWdata := core_io.data_wdata

        when(core_io.data_we) {
          // Stores always go to memory; the cache is updated on completion.
          state := CACHE_STATE.WRITE
        }.elsewhen(hit) {
          hitData := data(hitIdx) & expandBe(core_io.data_be)
          hitPending := true.B
        }.otherwise {
          state := CACHE_STATE.READ
        }
      }
    }

    is(CACHE_STATE.READ) {
      // Always fetch the full word so it can be cached.
      mem_io.data_req := true.B
      mem_io.data_addr := reqAddr
      mem_io.data_be := "b1111".U
      mem_io.data_we := false.B
      when(mem_io.data_gnt) {
        insert(reqAddr(31, 2), mem_io.data_rdata)
        core_io.data_gnt := true.B
        core_io.data_rdata := mem_io.data_rdata & expandBe(reqBe)
        state := CACHE_STATE.IDLE
      }
    }

    is(CACHE_STATE.WRITE) {
      mem_io.data_req := true.B
      mem_io.data_addr := reqAddr
      mem_io.data_be := reqBe
      mem_io.data_we := true.B
      mem_io.data_wdata := reqWdata
      when(mem_io.data_gnt) {
        core_io.data_gnt := true.B
        val hits = hitVecFor(reqAddr)
        val hit = hits.asUInt.orR
        val hitIdx = PriorityEncoder(hits)
        val mask = expandBe(reqBe)
        when(hit) {
          // Update the entry in place, its FIFO position does not change.
          data(hitIdx) := (mask & reqWdata) | (~mask & data(hitIdx))
        }.elsewhen(reqBe === "b1111".U) {
          // Only complete words can be inserted, partial writes would
          // leave the remaining bytes of the cache line undefined.
          insert(reqAddr(31, 2), reqWdata)
        }
        state := CACHE_STATE.IDLE
      }
    }
  }

  when(~io_reset.rst_n) {
    state := CACHE_STATE.IDLE
    valid := VecInit(Seq.fill(capacity)(false.B))
    fifoPtr := 0.U
    hitPending := false.B
  }
}
