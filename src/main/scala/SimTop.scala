import chisel3._
import difftest._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._

class SimTop(implicit p: Parameters) extends LazyModule {
  val icache      = LazyModule(new ICache(0, 32))
  val bridge_dmem = LazyModule(new CachePortToTileLinkBridge(1))
  val bridge_iptw = LazyModule(new CachePortToTileLinkBridge(2))
  val bridge_dptw = LazyModule(new CachePortToTileLinkBridge(3))
  val xbar        = LazyModule(new TLXbar)
  val mem         = LazyModule(new DiplomacyToAXI4Bridge())

  xbar.node := icache.node
  xbar.node := bridge_dmem.node
  xbar.node := bridge_iptw.node
  xbar.node := bridge_dptw.node

  (mem.node
    := AXI4UserYanker()
    := AXI4Deinterleaver(32)
    := TLToAXI4()
    := xbar.node)

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val logCtrl  = new LogCtrlIO
      val perfInfo = new PerfInfoIO
      val uart     = new UARTIO
      val memAXI_0 = new AXI4IO
    })

    val core = Module(new Core)

    icache.module.io.fence_i    := core.io.fence_i
    icache.module.io.cache      <> core.io.imem
    bridge_dmem.module.io.cache <> core.io.dmem
    bridge_iptw.module.io.cache <> core.io.iptw
    bridge_dptw.module.io.cache <> core.io.dptw
    io.memAXI_0                 <> mem.module.io.axi4

    io.uart.out.valid := false.B
    io.uart.out.ch    := 0.U
    io.uart.in.valid  := false.B
  }
}
