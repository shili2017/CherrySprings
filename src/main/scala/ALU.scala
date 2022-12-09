import chisel3._
import chisel3.util._
import Constant._

class ALU extends Module {
  val io = IO(new Bundle {
    val uop       = Input(new MicroOp)
    val in1       = Input(UInt(32.W))
    val in2       = Input(UInt(32.W))
    val out       = Output(UInt(32.W))
    val adder_out = Output(UInt(32.W))
    val cmp_out   = Output(Bool())
  })

  val is_sub       = io.uop.alu_op(3)
  val in2_inv      = Mux(is_sub, (~io.in2).asUInt, io.in2)
  val xor          = io.in1 ^ in2_inv
  val cmp_unsigned = io.uop.alu_op(1)
  val cmp_inverted = io.uop.alu_op(0)
  val cmp_eq       = !io.uop.alu_op(3)
  val lt           = Wire(Bool())

  lt           := Mux(io.in1(31) === io.in2(31), io.adder_out(31), Mux(cmp_unsigned, io.in2(31), io.in1(31)))
  io.adder_out := io.in1 + in2_inv + is_sub
  io.cmp_out   := cmp_inverted ^ Mux(cmp_eq, xor === 0.U, lt)

  val shamt = Wire(UInt(5.W))
  shamt := io.in2(4, 0).asUInt

  io.out := MuxLookup(
    io.uop.alu_op,
    0.U,
    Array(
      s"b$ALU_ADD".U  -> io.adder_out,
      s"b$ALU_SUB".U  -> io.adder_out,
      s"b$ALU_SLT".U  -> io.cmp_out.asUInt,
      s"b$ALU_SLTU".U -> io.cmp_out.asUInt,
      s"b$ALU_XOR".U  -> xor,
      s"b$ALU_OR".U   -> (io.in1 | io.in2).asUInt,
      s"b$ALU_AND".U  -> (io.in1 & io.in2).asUInt,
      s"b$ALU_SLL".U  -> ((io.in1 << shamt)(31, 0)).asUInt,
      s"b$ALU_SRL".U  -> (io.in1.asUInt >> shamt).asUInt,
      s"b$ALU_SRA".U  -> (io.in1.asSInt >> shamt).asUInt
    )
  )
}
