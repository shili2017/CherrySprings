import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._

class LSU(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 32 || xLen == 64)

  val io = IO(new Bundle {
    val uop    = Input(new MicroOp)
    val is_mem = Input(Bool())
    val addr   = Input(UInt(32.W))
    val wdata  = Input(UInt(xLen.W))
    val rdata  = Output(UInt(xLen.W))
    val valid  = Output(Bool())
    val dmem   = new CachePortIO
    val ready  = Output(Bool())
  })

  val uop      = io.uop
  val addr     = io.addr
  val wdata    = io.wdata
  val req      = io.dmem.req
  val resp     = io.dmem.resp
  val is_mem   = io.is_mem
  val is_store = uop.lsu_op === s"b$LSU_ST".U

  val s_idle :: s_req :: s_resp :: Nil = Enum(3)
  val state                            = RegInit(s_idle)

  val addr_offset = if (xLen == 32) addr(1, 0) else addr(2, 0)
  val wmask = if (xLen == 32) {
    MuxLookup(
      uop.mem_len,
      0.U,
      Array(
        s"b$MEM_BYTE".U -> "b0001".U(4.W),
        s"b$MEM_HALF".U -> "b0011".U(4.W),
        s"b$MEM_WORD".U -> "b1111".U(4.W)
      )
    )
  } else {
    MuxLookup(
      uop.mem_len,
      0.U,
      Array(
        s"b$MEM_BYTE".U  -> "b00000001".U(8.W),
        s"b$MEM_HALF".U  -> "b00000011".U(8.W),
        s"b$MEM_WORD".U  -> "b00001111".U(8.W),
        s"b$MEM_DWORD".U -> "b11111111".U(8.W)
      )
    )
  }

  req.bits.addr  := addr
  req.bits.wdata := (wdata << (addr_offset << 3))(xLen - 1, 0)
  req.bits.wmask := (wmask << addr_offset)(xLen / 8 - 1, 0)
  req.bits.wen   := is_store
  req.valid      := (state === s_req) && uop.valid && is_mem
  resp.ready     := (state === s_resp)

  val resp_data = resp.bits.rdata >> (addr_offset << 3)
  val ld_out    = Wire(UInt(xLen.W))
  val ldu_out   = Wire(UInt(xLen.W))

  switch(state) {
    is(s_idle) {
      when(is_mem) {
        state := s_req
      }
    }
    is(s_req) {
      when(req.fire) {
        state := s_resp
      }
    }
    is(s_resp) {
      when(resp.fire) {
        state := s_idle
      }
    }
  }

  if (xLen == 32) {
    ld_out := MuxLookup(
      uop.mem_len,
      0.U,
      Array(
        s"b$MEM_BYTE".U -> Cat(Fill(24, resp_data(7)), resp_data(7, 0)),
        s"b$MEM_HALF".U -> Cat(Fill(16, resp_data(15)), resp_data(15, 0)),
        s"b$MEM_WORD".U -> resp_data
      )
    )

    ldu_out := MuxLookup(
      uop.mem_len,
      0.U,
      Array(
        s"b$MEM_BYTE".U -> Cat(Fill(24, 0.U), resp_data(7, 0)),
        s"b$MEM_HALF".U -> Cat(Fill(16, 0.U), resp_data(15, 0))
      )
    )
  } else {
    ld_out := MuxLookup(
      uop.mem_len,
      0.U,
      Array(
        s"b$MEM_BYTE".U  -> Cat(Fill(56, resp_data(7)), resp_data(7, 0)),
        s"b$MEM_HALF".U  -> Cat(Fill(48, resp_data(15)), resp_data(15, 0)),
        s"b$MEM_WORD".U  -> Cat(Fill(32, resp_data(31)), resp_data(31, 0)),
        s"b$MEM_DWORD".U -> resp_data
      )
    )

    ldu_out := MuxLookup(
      uop.mem_len,
      0.U,
      Array(
        s"b$MEM_BYTE".U -> Cat(Fill(56, 0.U), resp_data(7, 0)),
        s"b$MEM_HALF".U -> Cat(Fill(48, 0.U), resp_data(15, 0)),
        s"b$MEM_WORD".U -> Cat(Fill(32, 0.U), resp_data(31, 0))
      )
    )
  }

  io.rdata := MuxLookup(
    uop.lsu_op,
    0.U,
    Array(
      s"b$LSU_LD".U  -> ld_out,
      s"b$LSU_LDU".U -> ldu_out
    )
  )
  io.valid := (state === s_resp) && resp.fire // assert for only 1 cycle
  io.ready := ((state === s_idle) && !is_mem) || io.valid
}
