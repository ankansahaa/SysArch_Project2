package RISCV.implementation.generic.caches

import chisel3._
import chisel3.util._
import RISCV.interfaces.generic.AbstractCache

class Prefetcher extends AbstractCache {
  val idle :: prefetch :: waitCore :: Nil = Enum(3)
  val state = RegInit(idle)

  val lastAddr = RegInit(VecInit(Seq.fill(3)(0.U(32.W))))
  val lastDiff = RegInit(0.U(32.W))
  val prefetchAddr = Reg(UInt(32.W))

  core_io.data_gnt := false.B
  core_io.data_rdata := 0.U

  mem_io.data_req := false.B
  mem_io.data_addr := 0.U
  mem_io.data_be := 0.U
  mem_io.data_we := false.B
  mem_io.data_wdata := 0.U

  val readReq =
    core_io.data_req && !core_io.data_we && core_io.data_gnt === false.B && state === idle
  when(readReq) {
    val diff = core_io.data_addr - lastAddr(0)
    when(
      lastAddr(1) =/= 0.U && lastAddr(0) =/= 0.U &&
        lastAddr(0) - lastAddr(1) === diff
    ) {
      lastDiff := diff
    }.otherwise {
      lastDiff := 0.U
    }
    lastAddr(2) := lastAddr(1)
    lastAddr(1) := lastAddr(0)
    lastAddr(0) := core_io.data_addr
  }

  val patternReady = lastDiff =/= 0.U && state === idle && !core_io.data_req
  when(patternReady) {
    prefetchAddr := lastAddr(0) + lastDiff
    lastDiff := 0.U
    state := prefetch
  }

  switch(state) {
    is(idle) {
      when(core_io.data_req) {
        mem_io.data_req := true.B
        mem_io.data_addr := core_io.data_addr
        mem_io.data_be := core_io.data_be
        mem_io.data_we := core_io.data_we
        mem_io.data_wdata := core_io.data_wdata
        when(mem_io.data_gnt) {
          core_io.data_gnt := true.B
          core_io.data_rdata := mem_io.data_rdata
        }
      }
    }

    is(prefetch) {
      mem_io.data_req := true.B
      mem_io.data_addr := prefetchAddr
      mem_io.data_be := "b1111".U
      mem_io.data_we := false.B
      when(mem_io.data_gnt) {
        state := idle
      }
      when(core_io.data_req) {
        state := waitCore
      }
    }

    is(waitCore) {
      mem_io.data_req := true.B
      mem_io.data_addr := prefetchAddr
      mem_io.data_be := "b1111".U
      mem_io.data_we := false.B
      when(mem_io.data_gnt) {
        state := idle
      }
    }
  }
}
