import chisel3.util._
import chisel3.util.experimental.decode._
import Constant._
import freechips.rocketchip.rocket.Instructions._

object HALT {
  def apply() = BitPat("b00000000000000000000000001101011")
}

object DecodeTable {
  val decode_default: String = Seq(
    //             v  i  fu      alu_op jmp_op    mdu_op lsu_op   lsu_len csr_op sys_op rs1/2_src rd_wen imm_type
                   N, Y, FU_ALU, ALU_X, JMP_NONE, MDU_X, LSU_NONE, LSU_X, CSR_X, SYS_X, RS_X, RS_X, N, IMM_X
  ).reduce(_ + _)

  val decode_table: TruthTable = TruthTable(Map(
    // RV32I
    LUI     -> Seq(Y, N, FU_ALU, ALU_ADD,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_ZERO, RS_IMM, Y, IMM_U),
    AUIPC   -> Seq(Y, N, FU_ALU, ALU_ADD,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_PC,   RS_IMM, Y, IMM_U),
    JAL     -> Seq(Y, N, FU_JMP, ALU_ADD,  JMP_JAL,  MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_PC,   RS_IMM, Y, IMM_J),
    JALR    -> Seq(Y, N, FU_JMP, ALU_ADD,  JMP_JALR, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    BEQ     -> Seq(Y, N, FU_JMP, ALU_SEQ,  JMP_BR,   MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  N, IMM_B),
    BNE     -> Seq(Y, N, FU_JMP, ALU_SNE,  JMP_BR,   MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  N, IMM_B),
    BLT     -> Seq(Y, N, FU_JMP, ALU_SLT,  JMP_BR,   MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  N, IMM_B),
    BGE     -> Seq(Y, N, FU_JMP, ALU_SGE,  JMP_BR,   MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  N, IMM_B),
    BLTU    -> Seq(Y, N, FU_JMP, ALU_SLTU, JMP_BR,   MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  N, IMM_B),
    BGEU    -> Seq(Y, N, FU_JMP, ALU_SGEU, JMP_BR,   MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  N, IMM_B),
    LB      -> Seq(Y, N, FU_LSU, ALU_ADD,  JMP_NONE, MDU_X,      LSU_LD,   LSU_BYTE,  CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    LH      -> Seq(Y, N, FU_LSU, ALU_ADD,  JMP_NONE, MDU_X,      LSU_LD,   LSU_HALF,  CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    LW      -> Seq(Y, N, FU_LSU, ALU_ADD,  JMP_NONE, MDU_X,      LSU_LD,   LSU_WORD,  CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    LBU     -> Seq(Y, N, FU_LSU, ALU_ADD,  JMP_NONE, MDU_X,      LSU_LDU,  LSU_BYTE,  CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    LHU     -> Seq(Y, N, FU_LSU, ALU_ADD,  JMP_NONE, MDU_X,      LSU_LDU,  LSU_HALF,  CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    SB      -> Seq(Y, N, FU_LSU, ALU_ADD,  JMP_NONE, MDU_X,      LSU_ST,   LSU_BYTE,  CSR_X,     SYS_X,      RS_RF,   RS_IMM, N, IMM_S),
    SH      -> Seq(Y, N, FU_LSU, ALU_ADD,  JMP_NONE, MDU_X,      LSU_ST,   LSU_HALF,  CSR_X,     SYS_X,      RS_RF,   RS_IMM, N, IMM_S),
    SW      -> Seq(Y, N, FU_LSU, ALU_ADD,  JMP_NONE, MDU_X,      LSU_ST,   LSU_WORD,  CSR_X,     SYS_X,      RS_RF,   RS_IMM, N, IMM_S),
    ADDI    -> Seq(Y, N, FU_ALU, ALU_ADD,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    SLTI    -> Seq(Y, N, FU_ALU, ALU_SLT,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    SLTIU   -> Seq(Y, N, FU_ALU, ALU_SLTU, JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    XORI    -> Seq(Y, N, FU_ALU, ALU_XOR,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    ORI     -> Seq(Y, N, FU_ALU, ALU_OR,   JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    ANDI    -> Seq(Y, N, FU_ALU, ALU_AND,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    SLLI    -> Seq(Y, N, FU_ALU, ALU_SLL,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    SRLI    -> Seq(Y, N, FU_ALU, ALU_SRL,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    SRAI    -> Seq(Y, N, FU_ALU, ALU_SRA,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_IMM, Y, IMM_I),
    ADD     -> Seq(Y, N, FU_ALU, ALU_ADD,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    SUB     -> Seq(Y, N, FU_ALU, ALU_SUB,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    SLL     -> Seq(Y, N, FU_ALU, ALU_SLL,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    SLT     -> Seq(Y, N, FU_ALU, ALU_SLT,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    SLTU    -> Seq(Y, N, FU_ALU, ALU_SLTU, JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    XOR     -> Seq(Y, N, FU_ALU, ALU_XOR,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    SRL     -> Seq(Y, N, FU_ALU, ALU_SRL,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    SRA     -> Seq(Y, N, FU_ALU, ALU_SRA,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    OR      -> Seq(Y, N, FU_ALU, ALU_OR,   JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    AND     -> Seq(Y, N, FU_ALU, ALU_AND,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    // CSR
    CSRRW   -> Seq(Y, N, FU_CSR, ALU_X,    JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_CSRRW, SYS_X,      RS_RF,   RS_X,   Y, IMM_X),
    CSRRS   -> Seq(Y, N, FU_CSR, ALU_X,    JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_CSRRS, SYS_X,      RS_RF,   RS_X,   Y, IMM_X),
    CSRRC   -> Seq(Y, N, FU_CSR, ALU_X,    JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_CSRRC, SYS_X,      RS_RF,   RS_X,   Y, IMM_X),
    CSRRWI  -> Seq(Y, N, FU_CSR, ALU_X,    JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_CSRRW, SYS_X,      RS_IMM,  RS_X,   Y, IMM_Z),
    CSRRSI  -> Seq(Y, N, FU_CSR, ALU_X,    JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_CSRRS, SYS_X,      RS_IMM,  RS_X,   Y, IMM_Z),
    CSRRCI  -> Seq(Y, N, FU_CSR, ALU_X,    JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_CSRRC, SYS_X,      RS_IMM,  RS_X,   Y, IMM_Z),
    // RV32M
    MUL     -> Seq(Y, N, FU_MDU, ALU_X,    JMP_NONE, MDU_MUL,    LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    MULH    -> Seq(Y, N, FU_MDU, ALU_X,    JMP_NONE, MDU_MULH,   LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    MULHSU  -> Seq(Y, N, FU_MDU, ALU_X,    JMP_NONE, MDU_MULHSU, LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    MULHU   -> Seq(Y, N, FU_MDU, ALU_X,    JMP_NONE, MDU_MULHU,  LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    DIV     -> Seq(Y, N, FU_MDU, ALU_X,    JMP_NONE, MDU_DIV,    LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    DIVU    -> Seq(Y, N, FU_MDU, ALU_X,    JMP_NONE, MDU_DIVU,   LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    REM     -> Seq(Y, N, FU_MDU, ALU_X,    JMP_NONE, MDU_REM,    LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    REMU    -> Seq(Y, N, FU_MDU, ALU_X,    JMP_NONE, MDU_REMU,   LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X),
    // HALT
    HALT()  -> Seq(Y, N, FU_ALU, ALU_ADD,  JMP_NONE, MDU_X,      LSU_NONE, LSU_X,     CSR_X,     SYS_X,      RS_RF,   RS_RF,  Y, IMM_X)
  ).map({ case (k, v) => k -> BitPat(s"b${v.reduce(_ + _)}") }), BitPat(s"b$decode_default"))
}
