import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._

class FDPacket(implicit p: Parameters) extends CherrySpringsBundle {
  val pc         = UInt(xLen.W)
  val instr      = UInt(32.W)
  val valid      = Bool()
  val page_fault = Bool()
}

class DXPacket(implicit p: Parameters) extends CherrySpringsBundle {
  val uop              = new MicroOp
  val rs1_data         = UInt(xLen.W)
  val rs2_data         = UInt(xLen.W)
  val rs2_data_from_rf = UInt(xLen.W)
}

class XMPacket(implicit p: Parameters) extends CherrySpringsBundle {
  val rs1_data         = UInt(xLen.W)
  val rs2_data_from_rf = UInt(xLen.W)
  val rd_data          = UInt(xLen.W)
  val uop              = new MicroOp
}

class MWPacket(implicit p: Parameters) extends CherrySpringsBundle {
  val uop     = new MicroOp
  val rd_data = UInt(xLen.W)
}

class PipelineReg[T <: Bundle](packet: T)(implicit p: Parameters) extends CherrySpringsModule {
  val io = IO(new Bundle {
    val in    = Input(packet)
    val out   = Output(packet)
    val en    = Input(Bool())
    val flush = Input(Bool())
  })

  val reg = RegEnable(Mux(io.flush, 0.U.asTypeOf(packet), io.in), 0.U.asTypeOf(packet), io.en)
  io.out := reg
}

class JmpPacket(implicit p: Parameters) extends CherrySpringsBundle {
  val valid  = Bool()
  val target = UInt(xLen.W)
}
