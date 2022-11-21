import chisel3._
import chisel3.util._

class IF0 extends Module {
  val io = IO(new Bundle {
    val jmp_packet = Input(new JmpPacket)
    val pc         = Output(UInt(32.W))
    val req        = Decoupled(new CachePortReq)
  })

  val jmp        = HoldUnless(io.jmp_packet.valid, io.jmp_packet.valid)
  val jmp_target = HoldUnless(io.jmp_packet.target, io.jmp_packet.valid)
  val pc         = RegInit("h80000000".U(32.W))
  val req_addr   = Mux(jmp, jmp_target, pc)

  when(io.req.fire) {
    pc := req_addr + 4.U
  }

  io.req.bits       := 0.U.asTypeOf(new CachePortReq)
  io.req.bits.addr  := req_addr
  io.req.valid      := true.B
  io.pc             := req_addr
}

class IF1 extends Module {
  val io = IO(new Bundle {
    val pc        = Input(UInt(32.W))
    val resp      = Flipped(Decoupled(new CachePortResp))
    val out       = Output(new FDPacket)
    val out_ready = Input(Bool())
  })

  io.resp.ready := io.out_ready
  io.out.pc     := io.pc
  io.out.instr  := io.resp.bits.rdata
  io.out.valid  := io.resp.fire
}

class IFU extends Module {
  val io = IO(new Bundle {
    val jmp_packet = Input(new JmpPacket)
    val imem       = new CachePortIO
    val out        = Output(new FDPacket)
    val out_ready  = Input(Bool())
  })

  val if0    = Module(new IF0)
  val if1    = Module(new IF1)
  val if1_pc = RegNext(if0.io.pc, 0.U(32.W))

  if0.io.req        <> io.imem.req
  if1.io.resp       <> io.imem.resp
  if0.io.jmp_packet := io.jmp_packet
  if1.io.pc         := if1_pc
  if1.io.out_ready  := io.out_ready
  io.out            := if1.io.out
}
