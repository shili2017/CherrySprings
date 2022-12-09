import chisel3._
import chisel3.util._

class IFU extends Module {
  val io = IO(new Bundle {
    val jmp_packet = Input(new JmpPacket)
    val imem       = new CachePortIO
    val out        = Output(new FDPacket)
    val out_ready  = Input(Bool())
  })

  val jmp = BoolStopWatch(io.jmp_packet.valid, io.imem.req.fire) || io.jmp_packet.valid
  val pc  = RegInit("h80000000".U(32.W))

  when(io.imem.resp.fire && !jmp) {
    pc := pc + 4.U
  }.elsewhen(io.jmp_packet.valid) {
    pc := io.jmp_packet.target
  }

  val busy = BoolStopWatch(io.imem.req.fire, io.imem.resp.fire)

  io.imem.req.bits      := 0.U.asTypeOf(new CachePortReq)
  io.imem.req.bits.addr := pc
  io.imem.req.valid     := !busy && io.out_ready
  io.imem.resp.ready    := RegNext(io.imem.resp.valid)

  val pc_queue = Module(new Queue(UInt(32.W), 2))
  pc_queue.io.enq.bits  := pc
  pc_queue.io.enq.valid := io.imem.req.fire
  pc_queue.io.deq.ready := io.out.valid || io.jmp_packet.valid

  val resp_data = HoldUnless(io.imem.resp.bits.rdata, io.imem.resp.fire && !jmp)
  val out_valid = BoolStopWatch(io.imem.resp.fire && !jmp, io.out.valid)

  io.out.pc    := pc_queue.io.deq.bits
  io.out.instr := resp_data
  io.out.valid := out_valid && io.out_ready
}
