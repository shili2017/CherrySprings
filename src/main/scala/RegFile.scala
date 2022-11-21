import chisel3._
import chipsalliance.rocketchip.config._
import chisel3.util.experimental.BoringUtils
import difftest._

class RegFile(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val rs1_index = Input(UInt(5.W))
    val rs2_index = Input(UInt(5.W))
    val rs1_data  = Output(UInt(32.W))
    val rs2_data  = Output(UInt(32.W))
    val rd_index  = Input(UInt(5.W))
    val rd_data   = Input(UInt(32.W))
    val rd_wen    = Input(Bool())
  })

  val rf = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

  when(io.rd_wen && (io.rd_index =/= 0.U)) {
    rf(io.rd_index) := io.rd_data;
  }

  io.rs1_data := Mux(io.rs1_index =/= 0.U, rf(io.rs1_index), 0.U)
  io.rs2_data := Mux(io.rs2_index =/= 0.U, rf(io.rs2_index), 0.U)

  when(io.rd_wen) {
    when(io.rd_index === io.rs1_index) {
      io.rs1_data := io.rd_data
    }
    when(io.rd_index === io.rs2_index) {
      io.rs2_data := io.rd_data
    }
  }

  if (p(EnableDifftest)) {
    val dt_ar = Module(new DifftestArchIntRegState)
    dt_ar.io.clock  := clock
    dt_ar.io.coreid := 0.U
    dt_ar.io.gpr    := rf
    BoringUtils.addSource(rf(10), "rf_a0")
  }
}
