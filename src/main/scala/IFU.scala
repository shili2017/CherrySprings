import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._

class IFU(implicit p: Parameters) extends CherrySpringsModule {
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
  val pc        = RegEnable(pc_next, resetPC.U, pc_update)
  pc_next := Mux(io.jmp_packet.valid, io.jmp_packet.target, pc + 4.U)

  io.imem.req.bits := 0.U.asTypeOf(new CachePortReq)
  if (xLen == 32) {
    io.imem.req.bits.addr := pc
  } else {
    io.imem.req.bits.addr := Cat(pc(31, 3), Fill(3, 0.U))
  }
  io.imem.req.valid  := io.out_ready
  io.imem.resp.ready := io.out_ready || io.jmp_packet.valid

  val pc_queue = Module(new Queue(UInt(32.W), 2))
  pc_queue.io.enq.bits  := pc
  pc_queue.io.enq.valid := io.imem.req.fire
  pc_queue.io.deq.ready := io.imem.resp.fire

  io.out.pc := pc_queue.io.deq.bits
  if (xLen == 32) {
    io.out.instr := io.imem.resp.bits.rdata
  } else {
    io.out.instr := Mux(io.out.pc(2), io.imem.resp.bits.rdata(63, 32), io.imem.resp.bits.rdata(31, 0))
  }
  io.out.valid := io.imem.resp.valid && !io.jmp_packet.valid && !jmp
}
