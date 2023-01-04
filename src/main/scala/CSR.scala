import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._
import difftest._

object PRV {
  val U = 0
  val S = 1
  val M = 3
}

// Based on RISC-V ISA Volume II: Privileged Architecture, Version 20211203
class CSR(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 64)

  val io = IO(new Bundle {
    val uop = Input(new MicroOp)
    val rw = new Bundle {
      val addr  = Input(UInt(12.W))
      val cmd   = Input(UInt(CSR_X.length.W))
      val wdata = Input(UInt(xLen.W))
      val rdata = Output(UInt(xLen.W))
    }
    val prv          = Output(UInt(2.W))
    val mprv         = Output(Bool())
    val mpp          = Output(UInt(2.W))
    val sv39_en      = Output(Bool())
    val satp_ppn     = Output(UInt(44.W))
    val fence_i      = Output(Bool())
    val jmp_packet   = Output(new JmpPacket)
    val lsu_addr     = Input(UInt(xLen.W))
    val lsu_exc_code = Input(UInt(4.W))
  })

  // privilege mode
  val prv = RegInit(PRV.M.U)

  val rdata = WireDefault(0.U(xLen.W))
  val wdata = Wire(UInt(xLen.W))
  val wen   = io.rw.cmd =/= s"b$CSR_N".U
  wdata := MuxLookup(
    io.rw.cmd,
    0.U,
    Array(
      s"b$CSR_RW".U -> io.rw.wdata,
      s"b$CSR_RS".U -> (rdata | io.rw.wdata),
      s"b$CSR_RC".U -> (rdata & ~io.rw.wdata)
    )
  )

  /*
   * Number:      0x100
   * Privilege:   SRW
   * Name:        sstatus
   * Description: Supervisor status register
   */
  val sstatus      = WireDefault(0.U(xLen.W))
  val sstatus_sie  = RegInit(0.U(1.W))
  val sstatus_spie = RegInit(0.U(1.W))
  val sstatus_ube  = 0.U(1.W)
  val sstatus_spp  = RegInit(0.U(1.W))
  val msstatus_vs  = 0.U(2.W)
  val msstatus_fs  = RegInit(0.U(2.W))
  val msstatus_xs  = 0.U(2.W)
  val sstatus_mprv = RegInit(0.U(1.W))
  val sstatus_sum  = RegInit(0.U(1.W))
  val sstatus_mxr  = RegInit(0.U(1.W))
  val sstatus_uxl  = log2Up(xLen / 16).U(2.W)
  val msstatus_sd  = msstatus_fs.orR
  sstatus := Cat(
    msstatus_sd,
    0.U(29.W),
    sstatus_uxl,
    0.U(12.W),
    sstatus_mxr,
    sstatus_sum,
    0.U(1.W),
    msstatus_xs,
    msstatus_fs,
    0.U(2.W),
    msstatus_vs,
    sstatus_spp,
    0.U(1.W),
    sstatus_ube,
    sstatus_spie,
    0.U(3.W),
    sstatus_sie,
    0.U(1.W)
  )
  when(io.rw.addr === 0x100.U) {
    rdata := sstatus
    when(wen) {
      sstatus_sie := wdata(1)
      msstatus_fs := wdata(14, 13)
    }
  }

  /*
   * Number:      0x105
   * Privilege:   SRW
   * Name:        stvec
   * Description: Supervisor trap-handler base address
   */
  val stvec = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x105.U) {
    rdata := stvec
    when(wen) {
      stvec := wdata
    }
  }

  /*
   * Number:      0x140
   * Privilege:   SRW
   * Name:        sscratch
   * Description: Scratch register for supervisor trap handlers
   */
  val sscratch = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x140.U) {
    rdata := sscratch
    when(wen) {
      sscratch := wdata
    }
  }

  /*
   * Number:      0x141
   * Privilege:   SRW
   * Name:        sepc
   * Description: Machine exception program counter
   */
  val sepc = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x141.U) {
    rdata := sepc
    when(wen) {
      sepc := wdata
    }
  }

  /*
   * Number:      0x142
   * Privilege:   SRW
   * Name:        scause
   * Description: Supervisor trap cause
   */
  val scause = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x142.U) {
    rdata := scause
    when(wen) {
      scause := wdata
    }
  }

  /*
   * Number:      0x143
   * Privilege:   SRW
   * Name:        stval
   * Description: Supervisor bad address or instruction
   */
  val stval = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x143.U) {
    rdata := stval
    when(wen) {
      stval := wdata
    }
  }

  /*
   * Number:      0x180
   * Privilege:   SRW
   * Name:        satp
   * Description: Supervisor address translation and protection
   */
  val satp              = RegInit(0.U(xLen.W))
  val satp_mode_updated = WireDefault(false.B)
  when(io.rw.addr === 0x180.U) {
    rdata := satp
    when(wen) {
      satp := wdata
      when(satp(63, 60) =/= wdata(63, 60)) {
        // refresh pipeline after satp mode is updated if not in M mode
        satp_mode_updated := (prv =/= PRV.M.U)
      }
    }
  }
  io.sv39_en  := (satp(63, 60) === 8.U)
  io.satp_ppn := satp(43, 0)

  /*
   * Number:      0xF14
   * Privilege:   MRO
   * Name:        mhartid
   * Description: Hardware thread ID
   */
  when(io.rw.addr === 0xf14.U) {
    rdata := hartID.U
  }

  /*
   * Number:      0x300
   * Privilege:   MRW
   * Name:        mstatus
   * Description: Machine status register
   */
  val mstatus      = WireDefault(0.U(xLen.W))
  val mstatus_sie  = RegInit(0.U(1.W))
  val mstatus_mie  = RegInit(0.U(1.W))
  val mstatus_spie = RegInit(0.U(1.W))
  val mstatus_ube  = 0.U(1.W)
  val mstatus_mpie = RegInit(0.U(1.W))
  val mstatus_spp  = RegInit(0.U(1.W))
  val mstatus_mpp  = RegInit(0.U(2.W))
  val mstatus_mprv = RegInit(0.U(1.W))
  val mstatus_sum  = RegInit(0.U(1.W))
  val mstatus_mxr  = RegInit(0.U(1.W))
  val mstatus_tvm  = RegInit(0.U(1.W))
  val mstatus_tw   = RegInit(0.U(1.W))
  val mstatus_tsr  = RegInit(0.U(1.W))
  val mstatus_uxl  = log2Up(xLen / 16).U(2.W)
  val mstatus_sxl  = log2Up(xLen / 16).U(2.W)
  val mstatus_sbe  = 0.U(1.W)
  val mstatus_mbe  = 0.U(1.W)
  mstatus := Cat(
    msstatus_sd,
    0.U(25.W),
    mstatus_mbe,
    mstatus_sbe,
    mstatus_sxl,
    mstatus_uxl,
    0.U(9.W),
    mstatus_tsr,
    mstatus_tw,
    mstatus_tvm,
    mstatus_mxr,
    mstatus_sum,
    mstatus_mprv,
    msstatus_xs,
    msstatus_fs,
    mstatus_mpp,
    msstatus_vs,
    mstatus_spp,
    mstatus_mpie,
    mstatus_ube,
    mstatus_spie,
    0.U(1.W),
    mstatus_mie,
    0.U(1.W),
    mstatus_sie,
    0.U(1.W)
  )
  when(io.rw.addr === 0x300.U) {
    rdata := mstatus
    when(wen) {
      mstatus_sie  := wdata(1)
      mstatus_mie  := wdata(3)
      mstatus_mpp  := wdata(12, 11)
      msstatus_fs  := wdata(14, 13)
      mstatus_mprv := wdata(17)
    }
  }
  io.mprv := mstatus_mprv
  io.mpp  := mstatus_mpp

  /*
   * Number:      0x302
   * Privilege:   MRW
   * Name:        medeleg
   * Description: Machine exception delegation register
   */
  val medeleg = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x302.U) {
    rdata := medeleg
    when(wen) {
      medeleg := wdata
    }
  }

  /*
   * Number:      0x303
   * Privilege:   MRW
   * Name:        mideleg
   * Description: Machine interrupt delegation register
   */
  val mideleg = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x303.U) {
    rdata := mideleg
    when(wen) {
      mideleg := wdata
    }
  }

  /*
   * Number:      0x304 / 0x104
   * Privilege:   MRW / SRW
   * Name:        mie / mie
   * Description: Machine / Supervisor interrupt-enable register
   */
  val mie      = WireDefault(0.U(xLen.W))
  val sie      = WireDefault(0.U(xLen.W))
  val mie_usie = RegInit(0.U(1.W))
  val mie_ssie = RegInit(0.U(1.W))
  val mie_msie = RegInit(0.U(1.W))
  val mie_utie = RegInit(0.U(1.W))
  val mie_stie = RegInit(0.U(1.W))
  val mie_mtie = RegInit(0.U(1.W))
  val mie_ueie = RegInit(0.U(1.W))
  val mie_seie = RegInit(0.U(1.W))
  val mie_meie = RegInit(0.U(1.W))
  mie := Cat(
    0.U(52.W),
    mie_meie,
    0.U(1.W),
    mie_seie,
    mie_ueie,
    mie_mtie,
    0.U(1.W),
    mie_stie,
    mie_utie,
    mie_msie,
    0.U(1.W),
    mie_ssie,
    mie_usie
  )
  sie := Cat(
    0.U(54.W),
    mie_seie,
    mie_ueie,
    0.U(2.W),
    mie_stie,
    mie_utie,
    0.U(2.W),
    mie_ssie,
    mie_usie
  )
  when(io.rw.addr === 0x304.U) {
    rdata := mie
    when(wen) {
      mie_usie := wdata(0)
      mie_ssie := wdata(1)
      mie_msie := wdata(3)
      mie_utie := wdata(4)
      mie_stie := wdata(5)
      mie_mtie := wdata(7)
      mie_ueie := wdata(8)
      mie_seie := wdata(9)
      mie_meie := wdata(11)
    }
  }
  when(io.rw.addr === 0x104.U) {
    rdata := sie
    when(wen) {
      mie_usie := wdata(0)
      mie_ssie := wdata(1)
      mie_utie := wdata(4)
      mie_stie := wdata(5)
      mie_ueie := wdata(8)
      mie_seie := wdata(9)
    }
  }

  /*
   * Number:      0x305
   * Privilege:   MRW
   * Name:        mtvec
   * Description: Machine trap-handler base address
   */
  val mtvec = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x305.U) {
    rdata := mtvec
    when(wen) {
      mtvec := wdata
    }
  }

  /*
   * Number:      0x340
   * Privilege:   MRW
   * Name:        mscratch
   * Description: Scratch register for machine trap handlers
   */
  val mscratch = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x340.U) {
    rdata := mscratch
    when(wen) {
      mscratch := wdata
    }
  }

  /*
   * Number:      0x341
   * Privilege:   MRW
   * Name:        mepc
   * Description: Machine exception program counter
   */
  val mepc = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x341.U) {
    rdata := mepc
    when(wen) {
      mepc := wdata
    }
  }

  /*
   * Number:      0x342
   * Privilege:   MRW
   * Name:        mcause
   * Description: Machine trap cause
   */
  val mcause = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x342.U) {
    rdata := mcause
    when(wen) {
      mcause := wdata
    }
  }

  /*
   * Number:      0x343
   * Privilege:   MRW
   * Name:        mtval
   * Description: Machine bad address or instruction
   */
  val mtval = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x343.U) {
    rdata := mtval
    when(wen) {
      mtval := wdata
    }
  }

  /*
   * Number:      0x344
   * Privilege:   MRW
   * Name:        mip
   * Description: Machine interrupt pending
   */
  val mip      = WireDefault(0.U(xLen.W))
  val sip      = WireDefault(0.U(xLen.W))
  val mip_usip = RegInit(0.U(1.W))
  val mip_ssip = RegInit(0.U(1.W))
  val mip_msip = RegInit(0.U(1.W))
  val mip_utip = RegInit(0.U(1.W))
  val mip_stip = RegInit(0.U(1.W))
  val mip_mtip = RegInit(0.U(1.W))
  val mip_ueip = RegInit(0.U(1.W))
  val mip_seip = RegInit(0.U(1.W))
  val mip_meip = RegInit(0.U(1.W))
  mip := Cat(
    0.U(52.W),
    mip_meip,
    0.U(1.W),
    mip_seip,
    mip_ueip,
    mip_mtip,
    0.U(1.W),
    mip_stip,
    mip_utip,
    mip_msip,
    0.U(1.W),
    mip_ssip,
    mip_usip
  )
  sip := Cat(
    0.U(54.W),
    mip_seip,
    mip_ueip,
    0.U(2.W),
    mip_stip,
    mip_utip,
    0.U(2.W),
    mip_ssip,
    mip_usip
  )
  when(io.rw.addr === 0x344.U) {
    rdata := mip
    when(wen) {
      mip_usip := wdata(0)
      mip_ssip := wdata(1)
      mip_msip := wdata(3)
      mip_utip := wdata(4)
      mip_stip := wdata(5)
      mip_mtip := wdata(7)
      mip_ueip := wdata(8)
      mip_seip := wdata(9)
      mip_meip := wdata(11)
    }
  }
  when(io.rw.addr === 0x144.U) {
    rdata := mip
    when(wen) {
      mip_usip := wdata(0)
      mip_ssip := wdata(1)
      mip_utip := wdata(4)
      mip_stip := wdata(5)
      mip_ueip := wdata(8)
      mip_seip := wdata(9)
    }
  }

  io.rw.rdata := rdata
  io.prv      := prv

  /*
   * An MRET or SRET instruction is used to return from a trap in M-mode or S-mode respectively.
   * When executing an xRET instruction, supposing xPP holds the value y, xIE is set to xPIE; the
   * privilege mode is changed to y; xPIE is set to 1; and xPP is set to the least-privileged
   * supported mode (U if U-mode is implemented, else M). If xPP != M, xRET also sets MPRV = 0.
   */
  val is_mret = io.uop.sys_op === s"b$SYS_MRET".U
  val is_sret = io.uop.sys_op === s"b$SYS_SRET".U

  when(is_mret) {
    prv          := mstatus_mpp
    mstatus_mie  := mstatus_mpie
    mstatus_mpie := 1.U
    mstatus_mpp  := PRV.U.U
    when(mstatus_mpp =/= PRV.M.U) {
      mstatus_mprv := 0.U
    }
  }

  when(is_sret) {
    prv          := mstatus_spp
    mstatus_sie  := mstatus_spie
    mstatus_spie := 1.U
    mstatus_spp  := PRV.U.U
    when(mstatus_spp =/= PRV.M.U) {
      mstatus_mprv := 0.U
    }
    sstatus_sie  := sstatus_spie
    sstatus_spie := 1.U
  }

  /*
   * Exception
   */
  val is_exc_from_prev = (io.uop.exc =/= s"b$EXC_N".U)
  val is_exc_from_lsu  = (io.lsu_exc_code =/= 0.U)
  val is_exc           = is_exc_from_prev || is_exc_from_lsu
  val cause = Mux(
    io.lsu_exc_code =/= 0.U,
    io.lsu_exc_code,
    MuxLookup(
      io.uop.exc,
      0.U,
      Array(
        s"b$EXC_IAM".U -> 0.U,
        s"b$EXC_IAF".U -> 1.U,
        s"b$EXC_II".U  -> 2.U,
        s"b$EXC_EC".U  -> Cat("b10".U, prv),
        s"b$EXC_IPF".U -> 12.U
      )
    )
  )
  when(prv === PRV.M.U && is_exc) {
    mepc         := io.uop.pc
    mcause       := cause
    mtval        := Mux(is_exc_from_lsu, io.lsu_addr, io.uop.pc)
    mstatus_mpie := mstatus_mie
    mstatus_mie  := 0.U
    mstatus_mpp  := PRV.M.U
  }
  // todo: exception from supervisor mode
  when(prv === PRV.U.U && is_exc) {
    sepc         := io.uop.pc
    scause       := cause
    stval        := Mux(is_exc_from_lsu, io.lsu_addr, io.uop.pc)
    sstatus_spie := sstatus_sie
    sstatus_sie  := 0.U
    mstatus_spie := mstatus_sie
    mstatus_sie  := 0.U
    prv          := PRV.S.U
  }

  // todo: this is temporary, fix it later
  when(io.rw.addr === 0x100.U || io.rw.addr === 0x300.U) {
    when(wen) {
      mstatus_sum := wdata(18)
      sstatus_sum := wdata(18)
    }
  }

  io.fence_i := io.uop.sys_op === s"b$SYS_FENCEI".U

  io.jmp_packet.valid := is_exc || io.fence_i || satp_mode_updated || is_mret || is_sret
  io.jmp_packet.target := Mux(
    is_exc,
    Mux(prv === PRV.U.U, stvec, mtvec),
    Mux(io.fence_i || satp_mode_updated, io.uop.npc, Mux(is_mret, mepc, sepc))
  )

  if (enableDifftest) {
    val diff_cs = Module(new DifftestCSRState)
    diff_cs.io.clock          := clock
    diff_cs.io.coreid         := hartID.U
    diff_cs.io.priviledgeMode := prv
    diff_cs.io.mstatus        := mstatus
    diff_cs.io.sstatus        := sstatus
    diff_cs.io.mepc           := mepc
    diff_cs.io.sepc           := sepc
    diff_cs.io.mtval          := mtval
    diff_cs.io.stval          := stval
    diff_cs.io.mtvec          := mtvec
    diff_cs.io.stvec          := stvec
    diff_cs.io.mcause         := mcause
    diff_cs.io.scause         := scause
    diff_cs.io.satp           := satp
    diff_cs.io.mip            := mip
    diff_cs.io.mie            := mie
    diff_cs.io.mscratch       := mscratch
    diff_cs.io.sscratch       := sscratch
    diff_cs.io.mideleg        := mideleg
    diff_cs.io.medeleg        := medeleg

    val diff_ae = Module(new DifftestArchEvent)
    diff_ae.io.clock         := clock
    diff_ae.io.coreid        := hartID.U
    diff_ae.io.intrNO        := 0.U
    diff_ae.io.cause         := RegNext(Mux(is_exc, cause, 0.U))
    diff_ae.io.exceptionPC   := RegNext(io.uop.pc)
    diff_ae.io.exceptionInst := 0.U
  }
}
