import chisel3._
import chisel3.util._
import Constant._
import freechips.rocketchip.rocket._

class MDU extends Module {
  val io = IO(new Bundle {
    val uop    = Input(new MicroOp)
    val is_mdu = Input(Bool())
    val in1    = Input(UInt(32.W))
    val in2    = Input(UInt(32.W))
    val out    = Output(UInt(32.W))
    val valid  = Output(Bool())
    val ready  = Output(Bool())
  })

  val uop    = io.uop
  val in1    = io.in1
  val in2    = io.in2
  val is_mdu = io.is_mdu

  val s_idle :: s_req :: s_resp :: Nil = Enum(3)
  val state                            = RegInit(s_idle)

  val rocket_mdu = Module(new MulDiv(MulDivParams(), 32))
  rocket_mdu.io.req.valid    := (state === s_req) && uop.valid && is_mdu
  rocket_mdu.io.req.bits.fn  := uop.mdu_op
  rocket_mdu.io.req.bits.dw  := false.B
  rocket_mdu.io.req.bits.in1 := in1
  rocket_mdu.io.req.bits.in2 := in2
  rocket_mdu.io.req.bits.tag := 0.U
  rocket_mdu.io.kill         := false.B
  rocket_mdu.io.resp.ready   := true.B

  switch(state) {
    is(s_idle) {
      when(is_mdu) {
        state := s_req
      }
    }
    is(s_req) {
      when(rocket_mdu.io.req.fire) {
        state := s_resp
      }
    }
    is(s_resp) {
      when(rocket_mdu.io.resp.fire) {
        state := s_idle
      }
    }
  }

  io.out   := rocket_mdu.io.resp.bits.data
  io.valid := (state === s_resp) && rocket_mdu.io.resp.fire // assert for only 1 cycle
  io.ready := ((state === s_idle) && !is_mdu) || io.valid
}
