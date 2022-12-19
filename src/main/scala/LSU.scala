import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._

class LSU(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 32 || xLen == 64)

  val io = IO(new Bundle {
    val uop      = Input(new MicroOp)
    val is_mem   = Input(Bool())
    val is_store = Input(Bool())
    val is_amo   = Input(Bool())
    val addr     = Input(UInt(32.W))
    val wdata    = Input(UInt(xLen.W))
    val rdata    = Output(UInt(xLen.W))
    val valid    = Output(Bool())
    val dmem     = new CachePortIO
    val ready    = Output(Bool())
  })

  // todo: implement lr/sc

  val uop      = io.uop
  val req      = io.dmem.req
  val resp     = io.dmem.resp
  val is_mem   = io.is_mem
  val is_store = io.is_store
  val is_amo   = io.is_amo

  val s_idle :: s_req :: s_resp :: s1      = Enum(7)
  val s_amo_ld_req :: s_amo_ld_resp :: s2  = s1
  val s_amo_st_req :: s_amo_st_resp :: Nil = s2
  val state                                = RegInit(s_idle)

  val addr_offset = if (xLen == 32) io.addr(1, 0) else io.addr(2, 0)
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

  val resp_data   = resp.bits.rdata >> (addr_offset << 3)
  val ld_out      = Wire(UInt(xLen.W))
  val ldu_out     = Wire(UInt(xLen.W))
  val ld_out_amo  = RegInit(0.U(xLen.W))
  val st_data     = (io.wdata << (addr_offset << 3))(xLen - 1, 0)
  val st_data_amo = Wire(UInt(xLen.W))

  req.bits.addr  := io.addr
  req.bits.wdata := Mux(state === s_req, st_data, st_data_amo)
  req.bits.wmask := (wmask << addr_offset)(xLen / 8 - 1, 0)
  req.bits.wen   := is_store || (state === s_amo_st_req)
  req.valid      := state === s_req || state === s_amo_ld_req || state === s_amo_st_req
  resp.ready     := state === s_resp || state === s_amo_ld_resp || state === s_amo_st_resp

  switch(state) {
    is(s_idle) {
      when(is_mem) {
        state := Mux(is_amo, s_amo_ld_req, s_req)
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
    is(s_amo_ld_req) {
      when(req.fire) {
        state := s_amo_ld_resp
      }
    }
    is(s_amo_ld_resp) {
      when(resp.fire) {
        state := s_amo_st_req
      }
    }
    is(s_amo_st_req) {
      when(req.fire) {
        state := s_amo_st_resp
      }
    }
    is(s_amo_st_resp) {
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

  when(state === s_amo_ld_resp && resp.fire) {
    ld_out_amo := ld_out
  }

  // todo: optimize this logic
  st_data_amo := MuxLookup(
    uop.lsu_op,
    0.U,
    Array(
      s"b$LSU_AMOSWAP".U -> io.wdata,
      s"b$LSU_AMOADD".U  -> (io.wdata + ld_out_amo),
      s"b$LSU_AMOAND".U  -> (io.wdata & ld_out_amo),
      s"b$LSU_AMOOR".U   -> (io.wdata | ld_out_amo),
      s"b$LSU_AMOXOR".U  -> (io.wdata ^ ld_out_amo),
      s"b$LSU_AMOMAX".U  -> Mux(io.wdata.asSInt > ld_out_amo.asSInt, io.wdata, ld_out_amo),
      s"b$LSU_AMOMAXU".U -> Mux(io.wdata.asUInt > ld_out_amo.asUInt, io.wdata, ld_out_amo),
      s"b$LSU_AMOMIN".U  -> Mux(io.wdata.asSInt < ld_out_amo.asSInt, io.wdata, ld_out_amo),
      s"b$LSU_AMOMINU".U -> Mux(io.wdata.asUInt < ld_out_amo.asUInt, io.wdata, ld_out_amo)
    )
  )

  io.rdata := Mux(is_amo, ld_out_amo, Mux(uop.lsu_op === s"b$LSU_LDU".U, ldu_out, ld_out))
  io.valid := (state === s_resp || state === s_amo_st_resp) && resp.fire // assert for only 1 cycle
  io.ready := ((state === s_idle) && !is_mem) || io.valid

  if (debugLoadStore) {
    when(io.dmem.req.fire) {
      when(io.dmem.req.bits.wen) {
        printf(
          "%d [STORE-REQ ] addr=%x wdata=%x wmask=%x\n",
          DebugTimer(),
          io.dmem.req.bits.addr,
          io.dmem.req.bits.wdata,
          io.dmem.req.bits.wmask
        )
      }.otherwise {
        printf(
          "%d [LOAD -REQ ] addr=%x\n",
          DebugTimer(),
          io.dmem.req.bits.addr
        )
      }
    }
    when(io.dmem.resp.fire) {
      printf(
        "%d [     -RESP] rdata=%x\n",
        DebugTimer(),
        io.dmem.resp.bits.rdata
      )
    }
  }
}
