import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._

class CachePortProxy(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 64)

  val io = IO(new Bundle {
    val prv      = Input(UInt(2.W))
    val sv39_en  = Input(Bool())
    val satp_ppn = Input(UInt(44.W))
    val in       = Flipped(new CachePortIO)
    val out      = new CachePortIO
    val ptw      = new CachePortIO
  })

  io.in.resp <> io.out.resp

  val s_in_req :: s_ptw_req :: s_ptw_resp :: s_out_req :: Nil = Enum(4)
  val state                                                   = RegInit(s_in_req)

  val in_req_bits = RegInit(0.U.asTypeOf(new CachePortReq))
  when(io.in.req.fire) {
    in_req_bits := io.in.req.bits
  }

  val ptw = Module(new PTW)
  ptw.io.satp_ppn   := io.satp_ppn
  ptw.io.ptw        <> io.ptw
  ptw.io.req.bits   := in_req_bits.addr
  ptw.io.req.valid  := (state === s_ptw_req)
  ptw.io.resp.ready := (state === s_ptw_resp)

  val need_translation = (io.prv =/= PRV.M.U) && io.sv39_en

  switch(state) {
    is(s_in_req) {
      when(io.in.req.fire) {
        state := Mux(need_translation, s_ptw_req, s_out_req)
      }
    }
    is(s_ptw_req) {
      when(ptw.io.req.fire) {
        state := s_ptw_resp
      }
    }
    is(s_ptw_resp) {
      when(ptw.io.resp.fire) {
        state := s_out_req
      }
    }
    is(s_out_req) {
      when(io.out.req.fire) {
        state := s_in_req
      }
    }
  }

  val paddr = RegEnable(ptw.io.resp.bits, 0.U, ptw.io.resp.fire)
  io.in.req.ready  := (state === s_in_req)
  io.out.req.valid := (state === s_out_req)
  io.out.req.bits  := in_req_bits
  when(need_translation) {
    io.out.req.bits.addr := paddr
  }
}
