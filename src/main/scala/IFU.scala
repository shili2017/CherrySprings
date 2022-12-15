import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._

class IFU(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val jmp_packet = Input(new JmpPacket)
    val imem       = new CachePortIO
    val out        = Output(new FDPacket)
    val out_ready  = Input(Bool())
  })

  val busy = BoolStopWatch(io.imem.req.fire, io.imem.resp.fire)
  val jmp = BoolStopWatch(
    io.jmp_packet.valid && (busy || io.imem.req.fire),
    (io.imem.resp.fire && !io.jmp_packet.valid) || RegNext(
      io.imem.resp.fire && !io.imem.req.fire && io.jmp_packet.valid
    )
  )

  val pc_next   = Wire(UInt(32.W))
  val pc_update = io.jmp_packet.valid || io.imem.req.fire
  val pc        = RegEnable(pc_next, p(ResetPC).U(32.W), pc_update)
  pc_next := Mux(io.jmp_packet.valid, io.jmp_packet.target, pc + 4.U)

  io.imem.req.bits      := 0.U.asTypeOf(new CachePortReq)
  io.imem.req.bits.addr := pc
  io.imem.req.valid     := io.out_ready
  io.imem.resp.ready    := io.out_ready || io.jmp_packet.valid

  val pc_queue = Module(new Queue(UInt(32.W), 2))
  pc_queue.io.enq.bits  := pc
  pc_queue.io.enq.valid := io.imem.req.fire
  pc_queue.io.deq.ready := io.imem.resp.fire

  // val resp_data = HoldUnless(io.imem.resp.bits.rdata, io.imem.resp.fire && !jmp)
  // val out_valid = BoolStopWatch(io.imem.resp.fire && !jmp, io.out.valid)

  io.out.pc    := pc_queue.io.deq.bits
  io.out.instr := io.imem.resp.bits.rdata
  io.out.valid := io.imem.resp.valid && !io.jmp_packet.valid && !jmp
}
