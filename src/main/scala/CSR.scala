import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._
import difftest._

class CSR(implicit p: Parameters) extends CherrySpringsModule {
  val io = IO(new Bundle {
    val sys_cmd = Input(UInt(SYS_X.length.W))
    val rw = new Bundle {
      val addr  = Input(UInt(12.W))
      val cmd   = Input(UInt(CSR_X.length.W))
      val wdata = Input(UInt(xLen.W))
      val rdata = Output(UInt(xLen.W))
    }
    val fence_i = Output(Bool())
  })

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

  val mtvec = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x305.U) {
    rdata := mtvec
    when(wen) {
      mtvec := wdata
    }
  }

  val mepc = RegInit(0.U(xLen.W))
  when(io.rw.addr === 0x341.U) {
    rdata := mepc
    when(wen) {
      mepc := wdata
    }
  }

  val mstatus = RegInit(0.U(xLen.W))

  io.rw.rdata := rdata

  val mode = RegInit(3.U(2.W))
  when(io.sys_cmd === s"b$SYS_MRET".U) {
    mode    := 0.U
    mstatus := 0x80.U
  }

  io.fence_i := io.sys_cmd === s"b$SYS_FENCEI".U

  if (enableDifftest) {
    val diff_cs = Module(new DifftestCSRState)
    diff_cs.io.clock          := clock
    diff_cs.io.coreid         := 0.U
    diff_cs.io.priviledgeMode := mode
    diff_cs.io.mstatus        := mstatus
    diff_cs.io.sstatus        := 0.U
    diff_cs.io.mepc           := mepc
    diff_cs.io.sepc           := 0.U
    diff_cs.io.mtval          := 0.U
    diff_cs.io.stval          := 0.U
    diff_cs.io.mtvec          := mtvec
    diff_cs.io.stvec          := 0.U
    diff_cs.io.mcause         := 0.U
    diff_cs.io.scause         := 0.U
    diff_cs.io.satp           := 0.U
    diff_cs.io.mip            := 0.U
    diff_cs.io.mie            := 0.U
    diff_cs.io.mscratch       := 0.U
    diff_cs.io.sscratch       := 0.U
    diff_cs.io.mideleg        := 0.U
    diff_cs.io.medeleg        := 0.U
  }
}
