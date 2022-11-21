import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._

class CachePortToTileLinkBridge(source: Int)(implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            name     = s"CachePort$source",
            sourceId = IdRange(0, 7)
          )
        )
      )
    )
  )

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val cache = Flipped(new CachePortIO)
    })
    val (tl, edge) = node.out.head

    val req  = io.cache.req
    val resp = io.cache.resp

    tl.a.valid := req.valid
    req.ready  := tl.a.ready
    resp.valid := tl.d.valid
    tl.d.ready := resp.ready

    // 32-bit CachePortIO to 64-bit TileLink/AXI4
    val req_addr   = Cat(req.bits.addr(31, 3), Fill(3, 0.U))
    val req_wdata  = Mux(req_addr(2), Cat(req.bits.wdata, 0.U(32.W)), Cat(0.U(32.W), req.bits.wdata))
    val req_wmask  = Mux(req_addr(2), Cat(req.bits.wmask, 0.U(4.W)), Cat(0.U(4.W), req.bits.wmask))
    val req_addr_2 = RegEnable(req_addr(2).asBool, false.B, req.fire)

    val (_, get_bits) = edge.Get(source.U, req_addr, 3.U)
    val (_, put_bits) = edge.Put(source.U, req_addr, 3.U, req_wdata, req_wmask)

    tl.a.bits       := Mux(req.bits.wen, put_bits, get_bits)
    resp.bits.rdata := Mux(req_addr_2, tl.d.bits.data(63, 32), tl.d.bits.data(31, 0))
  }
}

class DiplomacyToAXI4Bridge(implicit p: Parameters) extends LazyModule {
  val device    = new SimpleDevice("memory", Seq("memory"))
  val beatBytes = 8
  val node = AXI4SlaveNode(
    Seq(
      AXI4SlavePortParameters(
        Seq(
          AXI4SlaveParameters(
            address       = Seq(AddressSet(BigInt("080000000", 16), BigInt("7fffffff", 16))),
            resources     = device.reg,
            regionType    = RegionType.UNCACHED,
            executable    = true,
            supportsWrite = TransferSizes(1, beatBytes),
            supportsRead  = TransferSizes(1, beatBytes)
          )
        ),
        beatBytes  = beatBytes,
        minLatency = 0
      )
    )
  )

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val memAXI_0 = new AXI4IO
    })

    val (axi, _) = node.in.head
    io.memAXI_0.awaddr  := axi.aw.bits.addr
    io.memAXI_0.awlen   := axi.aw.bits.len
    io.memAXI_0.awsize  := axi.aw.bits.size
    io.memAXI_0.awburst := axi.aw.bits.burst
    io.memAXI_0.awprot  := axi.aw.bits.prot
    io.memAXI_0.awlock  := axi.aw.bits.lock
    io.memAXI_0.awcache := axi.aw.bits.cache
    io.memAXI_0.awqos   := axi.aw.bits.qos
    io.memAXI_0.awid    := axi.aw.bits.id
    io.memAXI_0.awuser  := 0.U
    io.memAXI_0.awvalid := axi.aw.valid
    axi.aw.ready        := io.memAXI_0.awready

    io.memAXI_0.wdata  := axi.w.bits.data
    io.memAXI_0.wstrb  := axi.w.bits.strb
    io.memAXI_0.wlast  := axi.w.bits.last
    io.memAXI_0.wuser  := 0.U
    io.memAXI_0.wvalid := axi.w.valid
    axi.w.ready        := io.memAXI_0.wready

    axi.b.bits.resp    := io.memAXI_0.bresp
    axi.b.bits.id      := io.memAXI_0.bid
    axi.b.valid        := io.memAXI_0.bvalid
    io.memAXI_0.bready := axi.b.ready

    io.memAXI_0.araddr  := axi.ar.bits.addr
    io.memAXI_0.arlen   := axi.ar.bits.len
    io.memAXI_0.arsize  := axi.ar.bits.size
    io.memAXI_0.arburst := axi.ar.bits.burst
    io.memAXI_0.arprot  := axi.ar.bits.prot
    io.memAXI_0.arlock  := axi.ar.bits.lock
    io.memAXI_0.arcache := axi.ar.bits.cache
    io.memAXI_0.arqos   := axi.ar.bits.qos
    io.memAXI_0.arid    := axi.ar.bits.id
    io.memAXI_0.aruser  := 0.U
    io.memAXI_0.arvalid := axi.ar.valid
    axi.ar.ready        := io.memAXI_0.arready

    axi.r.bits.resp    := io.memAXI_0.rresp
    axi.r.bits.data    := io.memAXI_0.rdata
    axi.r.bits.last    := io.memAXI_0.rlast
    axi.r.bits.id      := io.memAXI_0.rid
    axi.r.valid        := io.memAXI_0.rvalid
    io.memAXI_0.rready := axi.r.ready
  }
}
