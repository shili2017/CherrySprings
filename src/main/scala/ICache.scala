import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._

class ICache(source: Int, size: Int)(implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            name            = s"InstructionCache$source",
            sourceId        = IdRange(0, 7),
            supportsProbe   = TransferSizes(32),
            supportsGet     = TransferSizes(32),
            supportsPutFull = TransferSizes(32)
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

    def getOffset(x: UInt) = x(4, 2)
    def getIndex(x:  UInt) = x(4 + log2Up(size), 5)
    def getTag(x:    UInt) = x(31, 5 + log2Up(size))

    val req  = io.cache.req
    val resp = io.cache.resp

    // Directed-mapped, #size sets, 8-word cacheline, read-only
    val array        = Module(new SRAM(size))
    val valid        = RegInit(VecInit(Seq.fill(size)(false.B)))
    val tag          = RegInit(VecInit(Seq.fill(size)(0.U((32 - 5 - log2Up(size)).W))))
    val hit          = valid(getIndex(req.bits.addr)) && (getTag(req.bits.addr) === tag(getIndex(req.bits.addr)))
    val hit_r        = RegEnable(hit, false.B, req.fire)
    val addr_r       = RegEnable(req.bits.addr, 0.U, req.fire)
    val refill_count = RegInit(0.U(2.W)) // saturation counter (0 -> 1 -> 2 -> 3 -> 0)

    val s_check :: s_req :: s_resp :: s_ok :: Nil = Enum(4)
    val state                                     = RegInit(s_check)

    when(tl.d.fire) {
      refill_count := refill_count + 1.U
    }

    switch(state) {
      is(s_check) {
        when(req.fire) {
          state := Mux(hit, s_ok, s_req)
        }
      }
      is(s_req) {
        when(tl.a.fire) {
          state := s_resp
        }
      }
      is(s_resp) {
        when(tl.d.fire && refill_count === 3.U) {
          state := s_ok
        }
      }
      is(s_ok) {
        when(resp.fire) {
          state := s_check
        }
      }
    }

    val (_, get_bits) = edge.Get(source.U, Cat(addr_r(31, 5), Fill(5, 0.U)), 5.U)

    val data_from_cache = HoldUnless(
      MuxLookup(
        getOffset(addr_r),
        0.U(32.W),
        Array(
          0.U -> array.io.rdata(31, 0),
          1.U -> array.io.rdata(63, 32),
          2.U -> array.io.rdata(95, 64),
          3.U -> array.io.rdata(127, 96),
          4.U -> array.io.rdata(159, 128),
          5.U -> array.io.rdata(191, 160),
          6.U -> array.io.rdata(223, 192),
          7.U -> array.io.rdata(255, 224)
        )
      ),
      RegNext(req.fire)
    )
    val data_from_memory = RegEnable(
      Mux(addr_r(2), tl.d.bits.data(63, 32), tl.d.bits.data(31, 0)),
      0.U(32.W),
      tl.d.fire && refill_count === addr_r(4, 3)
    )

    array.io.addr  := Mux(state === s_check, getIndex(io.cache.req.bits.addr), getIndex(addr_r))
    array.io.wen   := tl.d.fire
    array.io.widx  := refill_count
    array.io.wdata := tl.d.bits.data

    when(resp.fire) {
      valid(getIndex(addr_r)) := true.B
      tag(getIndex(addr_r))   := getTag(addr_r)
    }

    req.ready       := state === s_check
    tl.a.valid      := state === s_req
    tl.a.bits       := get_bits
    tl.d.ready      := state === s_resp
    resp.valid      := state === s_ok
    resp.bits.rdata := Mux(hit_r, data_from_cache, data_from_memory)
  }
}
