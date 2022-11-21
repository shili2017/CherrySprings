import chisel3._
import chisel3.util._

class FDPacket extends Bundle {
  val pc    = UInt(32.W)
  val instr = UInt(32.W)
  val valid = Bool()
}

class DXPacket extends Bundle {
  val uop      = new MicroOp
  val rs1_data = UInt(32.W)
  val rs2_data = UInt(32.W)
}

class XMPacket extends Bundle {
  val rs2_data = UInt(32.W)
  val rd_data  = UInt(32.W)
  val uop      = new MicroOp
}

class MWPacket extends Bundle {
  val uop     = new MicroOp
  val rd_data = UInt(32.W)
}

class PipelineReg[T <: Bundle](packet: T) extends Module {
  val io = IO(new Bundle {
    val in    = Input(packet)
    val out   = Output(packet)
    val en    = Input(Bool())
    val flush = Input(Bool())
  })

  val reg = RegEnable(Mux(io.flush, 0.U.asTypeOf(packet), io.in), 0.U.asTypeOf(packet), io.en)
  io.out := reg
}

class JmpPacket extends Bundle {
  val valid  = Bool()
  val target = UInt(32.W)
}
