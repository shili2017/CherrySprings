import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._

class AddrTransPortReq(implicit p: Parameters) extends CherrySpringsBundle {
  val vaddr = Output(UInt(xLen.W))
}

class AddrTransPortResp(implicit p: Parameters) extends CherrySpringsBundle {
  val paddr      = Output(UInt(xLen.W))
  val page_fault = Output(Bool())
}

class AddrTransPortIO(implicit p: Parameters) extends CherrySpringsBundle {
  val req  = Decoupled(new AddrTransPortReq)
  val resp = Flipped(Decoupled(new AddrTransPortResp))
}

class PTW(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 64)
  val PageTableLevels = 3

  val io = IO(new Bundle {
    val satp_ppn   = Input(UInt(44.W))
    val addr_trans = Flipped(new AddrTransPortIO)
    val ptw        = new CachePortIO
  })

  val s_req :: s_resp :: s_ptw_req :: s_ptw_resp :: Nil = Enum(4)
  val state                                             = RegInit(s_req)

  val pt_level   = RegInit(0.U(2.W))
  val vaddr      = RegInit(0.U(xLen.W))
  val pt_rdata   = RegInit(0.U(xLen.W))
  val pte_valid  = io.ptw.resp.bits.rdata(0)
  val pte_xwr    = io.ptw.resp.bits.rdata(4, 2)
  val page_fault = RegInit(false.B)

  assert(pt_level < PageTableLevels.U)

  switch(state) {
    is(s_req) {
      when(io.addr_trans.req.fire) {
        state := s_ptw_req
      }
    }
    is(s_ptw_req) {
      when(io.ptw.req.fire) {
        state := s_ptw_resp
      }
    }
    is(s_ptw_resp) {
      when(io.ptw.resp.fire) {
        when(!pte_valid || (pte_valid && (pte_xwr =/= 0.U))) {
          state := s_resp
        }.otherwise {
          state    := s_ptw_req
          pt_level := pt_level - 1.U
        }
      }
    }
    is(s_resp) {
      when(io.addr_trans.resp.fire) {
        state := s_req
      }
    }
  }

  when(io.addr_trans.req.fire) {
    pt_level   := (PageTableLevels - 1).U
    vaddr      := io.addr_trans.req.bits.vaddr
    page_fault := false.B
  }

  when(io.ptw.resp.fire) {
    pt_rdata := io.ptw.resp.bits.rdata
    when(!pte_valid) {
      page_fault := true.B
    }
  }

  val l2_addr = Cat(io.satp_ppn, vaddr(38, 30), 0.U(3.W))
  val l1_addr = Cat(pt_rdata(53, 10), vaddr(29, 21), 0.U(3.W))
  val l0_addr = Cat(pt_rdata(53, 10), vaddr(20, 12), 0.U(3.W))

  io.addr_trans.req.ready            := (state === s_req)
  io.addr_trans.resp.valid           := (state === s_resp)
  io.addr_trans.resp.bits.page_fault := page_fault
  io.addr_trans.resp.bits.paddr := MuxLookup(
    pt_level,
    0.U,
    Array(
      2.U -> Cat(pt_rdata(53, 28), vaddr(29, 0)),
      1.U -> Cat(pt_rdata(53, 19), vaddr(20, 0)),
      0.U -> Cat(pt_rdata(53, 10), vaddr(11, 0))
    )
  )

  io.ptw.req.bits := 0.U.asTypeOf(new CachePortReq)
  io.ptw.req.bits.addr := MuxLookup(
    pt_level,
    0.U,
    Array(
      2.U -> l2_addr,
      1.U -> l1_addr,
      0.U -> l0_addr
    )
  )
  io.ptw.req.valid  := (state === s_ptw_req)
  io.ptw.resp.ready := (state === s_ptw_resp)

  if (debugPTW) {
    when(io.addr_trans.req.fire) {
      printf("%d [PTW] vaddr=%x satp_ppn=%x\n", DebugTimer(), io.addr_trans.req.bits.vaddr, io.satp_ppn)
    }
    when(io.ptw.resp.fire) {
      printf(
        "%d [PTW] level=%d pte=%x ppn=%x\n",
        DebugTimer(),
        pt_level,
        io.ptw.resp.bits.rdata,
        io.ptw.resp.bits.rdata(53, 10)
      )
    }
    when(io.addr_trans.resp.fire) {
      printf(
        "%d [PTW] paddr=%x page_fault=%x\n",
        DebugTimer(),
        io.addr_trans.resp.bits.paddr,
        io.addr_trans.resp.bits.page_fault
      )
    }
  }
}
