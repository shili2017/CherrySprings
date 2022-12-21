import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._

class PTW(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 64)
  val PageTableLevels = 3

  val io = IO(new Bundle {
    val satp_ppn = Input(UInt(44.W))
    val req      = Flipped(Decoupled(UInt(xLen.W))) // virtual address
    val resp     = Decoupled(UInt(xLen.W)) // physical address
    val ptw      = new CachePortIO
  })

  val s_req :: s_resp :: s_ptw_req :: s_ptw_resp :: Nil = Enum(4)
  val state                                             = RegInit(s_req)

  val pt_level = RegInit(0.U(2.W))
  val vaddr    = RegInit(0.U(xLen.W))
  val pt_rdata = RegInit(0.U(xLen.W))

  switch(state) {
    is(s_req) {
      when(io.req.fire) {
        state := s_ptw_req
      }
    }
    is(s_ptw_req) {
      when(io.ptw.req.fire) {
        state := s_ptw_resp
      }
    }
    is(s_ptw_resp) {
      when(io.ptw.resp.fire) {
        state := Mux(pt_level === 0.U, s_resp, s_ptw_req)
      }
    }
    is(s_resp) {
      when(io.resp.fire) {
        state := s_req
      }
    }
  }

  when(io.req.fire) {
    pt_level := (PageTableLevels - 1).U
    vaddr    := io.req.bits
  }

  when(io.ptw.resp.fire) {
    pt_level := pt_level - 1.U
    pt_rdata := io.ptw.resp.bits.rdata
  }

  val l2_addr = Cat(io.satp_ppn, vaddr(38, 30), 0.U(3.W))
  val l1_addr = Cat(pt_rdata(53, 10), vaddr(29, 21), 0.U(3.W))
  val l0_addr = Cat(pt_rdata(53, 10), vaddr(20, 12), 0.U(3.W))

  io.req.ready    := (state === s_req)
  io.resp.valid   := (state === s_resp)
  io.resp.bits    := Cat(pt_rdata(53, 10), vaddr(11, 0))
  io.ptw.req.bits := 0.U.asTypeOf(new CachePortReq)
  io.ptw.req.bits.addr := MuxLookup(
    pt_level,
    0.U,
    Array(
      2.U -> l2_addr,
      1.U -> l1_addr,
      0.U -> l0_addr
    )
  )
  io.ptw.req.valid  := (state === s_ptw_req)
  io.ptw.resp.ready := (state === s_ptw_resp)
}
