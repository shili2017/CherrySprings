import chisel3._
import difftest._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._

class SimTop(implicit p: Parameters) extends LazyModule {
  val bridge_imem = LazyModule(new CachePortToTileLinkBridge(0))
  val bridge_dmem = LazyModule(new CachePortToTileLinkBridge(1))
  val xbar        = LazyModule(new TLXbar)
  val mem         = LazyModule(new DiplomacyToAXI4Bridge())

  xbar.node := bridge_imem.node
  xbar.node := bridge_dmem.node

  (mem.node
    := AXI4UserYanker()
    := AXI4Deinterleaver(32)
    := TLToAXI4()
    := xbar.node)

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val logCtrl               = new LogCtrlIO
      val perfInfo              = new PerfInfoIO
      val uart                  = new UARTIO
      val memAXI_0              = new AXI4IO
    })

    val core = Module(new Core)

    bridge_imem.module.io.cache <> core.io.imem
    bridge_dmem.module.io.cache <> core.io.dmem
    io.memAXI_0                 <> mem.module.io.memAXI_0

    io.uart.out.valid        := false.B
    io.uart.out.ch           := 0.U
    io.uart.in.valid         := false.B
  }
}
