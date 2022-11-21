import chisel3._
import chipsalliance.rocketchip.config._
import Constant._
import chisel3.util.experimental.BoringUtils
import difftest._

class Core(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val imem = new CachePortIO
    val dmem = new CachePortIO
  })

  val stall_b = WireInit(false.B)
  val flush   = WireInit(false.B)

  /* ----- Stage 1 - Instruction Fetch 1 (IF1) ----- */

  val ifu        = Module(new IFU)
  val jmp_packet = Wire(new JmpPacket)
  ifu.io.imem       <> io.imem
  ifu.io.out_ready  := stall_b
  ifu.io.jmp_packet := jmp_packet

  val if_id = Module(new PipelineReg(new FDPacket))
  if_id.io.in    <> ifu.io.out
  if_id.io.en    := stall_b
  if_id.io.flush := flush

  /* ----- Stage 2 - Instruction Decode (ID) ------- */

  val decode = Module(new Decode)
  decode.io.in <> if_id.io.out

  val rf = Module(new RegFile)
  rf.io.rs1_index := decode.io.out.rs1_index
  rf.io.rs2_index := decode.io.out.rs2_index

  val id_rs1_data = Wire(UInt(32.W))
  val id_rs2_data = Wire(UInt(32.W))
  val id_ex       = Module(new PipelineReg(new DXPacket))
  id_ex.io.in.uop      := decode.io.out
  id_ex.io.in.rs1_data := id_rs1_data
  id_ex.io.in.rs2_data := id_rs2_data
  id_ex.io.en          := stall_b
  id_ex.io.flush       := flush

  /* ----- Stage 3 - Execution (EX) ---------------- */

  val alu = Module(new ALU)
  alu.io.uop        := id_ex.io.out.uop
  alu.io.in1        := id_ex.io.out.rs1_data
  alu.io.in2        := id_ex.io.out.rs2_data
  jmp_packet.valid  := alu.io.uop.fu === s"b$FU_JMP".U && alu.io.cmp_out
  jmp_packet.target := alu.io.adder_out

  val ex_mem = Module(new PipelineReg(new XMPacket))
  ex_mem.io.in.uop      := id_ex.io.out.uop
  ex_mem.io.in.rs2_data := id_ex.io.out.rs2_data
  ex_mem.io.in.rd_data  := alu.io.out
  ex_mem.io.en          := stall_b
  ex_mem.io.flush       := flush

  /* ----- Stage 4 - Memory (MEM) ------------------ */

  val lsu = Module(new LSU)
  lsu.io.uop   := ex_mem.io.out.uop
  lsu.io.addr  := ex_mem.io.out.rd_data
  lsu.io.wdata := ex_mem.io.out.rs2_data
  lsu.io.dmem  <> io.dmem

  val is_load = ex_mem.io.out.uop.lsu_op === s"b$LSU_LD".U || ex_mem.io.out.uop.lsu_op === s"b$LSU_LDU".U
  val mem_wb  = Module(new PipelineReg(new MWPacket))
  mem_wb.io.in.uop     := id_ex.io.out.uop
  mem_wb.io.in.rd_data := Mux(is_load, lsu.io.rdata, ex_mem.io.out.rd_data)
  mem_wb.io.en         := lsu.io.ready
  mem_wb.io.flush      := false.B

  /* ----- Stage 5 - Write Back (WB) --------------- */

  val commit_uop = mem_wb.io.out.uop
  rf.io.rd_wen   := commit_uop.rd_wen
  rf.io.rd_index := commit_uop.rd_index
  rf.io.rd_data  := mem_wb.io.out.rd_data

  /* ----- Forwarding Unit ------------------------- */

  val need_rs1 = decode.io.out.rs1_src === s"b$RS_RF".U
  val need_rs2 = decode.io.out.rs2_src === s"b$RS_RF".U || decode.io.out.lsu_op === s"b$LSU_ST".U

  when(
    need_rs1 && id_ex.io.out.uop.rd_wen
      && decode.io.out.rs1_index === id_ex.io.out.uop.rd_index
      && decode.io.out.rs1_index =/= 0.U
  ) {
    id_rs1_data := ex_mem.io.in.rd_data
  }.elsewhen(
    need_rs1 && ex_mem.io.out.uop.rd_wen
      && decode.io.out.rs1_index === ex_mem.io.out.uop.rd_index
      && decode.io.out.rs1_index =/= 0.U
  ) {
    id_rs1_data := mem_wb.io.in.rd_data
  }.otherwise {
    id_rs1_data := rf.io.rs1_data
  }

  when(
    need_rs2 && id_ex.io.out.uop.rd_wen
      && decode.io.out.rs2_index === id_ex.io.out.uop.rd_index
      && decode.io.out.rs2_index =/= 0.U
  ) {
    id_rs2_data := ex_mem.io.in.rd_data
  }.elsewhen(
    need_rs2 && ex_mem.io.out.uop.rd_wen
      && decode.io.out.rs2_index === ex_mem.io.out.uop.rd_index
      && decode.io.out.rs2_index =/= 0.U
  ) {
    id_rs2_data := mem_wb.io.in.rd_data
  }.otherwise {
    id_rs2_data := rf.io.rs2_data
  }

  /* ----- Pipeline Control Signals -------------- */

  stall_b := lsu.io.ready
  flush   := jmp_packet.valid

  /* ----- Performance Counters ------------------ */

  val cycle_cnt = RegInit(UInt(64.W), 0.U)
  val instr_cnt = RegInit(UInt(64.W), 0.U)

  cycle_cnt := cycle_cnt + 1.U
  instr_cnt := instr_cnt + commit_uop.valid.asUInt

  if (p(EnableDifftest)) {
    val diff_ic = Module(new DifftestInstrCommit)
    diff_ic.io.clock   := clock
    diff_ic.io.coreid  := 0.U
    diff_ic.io.index   := 0.U
    diff_ic.io.pc      := commit_uop.pc
    diff_ic.io.instr   := commit_uop.instr
    diff_ic.io.valid   := commit_uop.valid
    diff_ic.io.special := false.B
    diff_ic.io.skip    := false.B
    diff_ic.io.isRVC   := false.B
    diff_ic.io.rfwen   := commit_uop.rd_wen
    diff_ic.io.fpwen   := false.B
    diff_ic.io.wpdest  := commit_uop.rd_index
    diff_ic.io.wdest   := commit_uop.rd_index

    val trap  = (commit_uop.instr === "h0000006b".U) && commit_uop.valid
    val rf_a0 = WireInit(0.U(64.W))
    BoringUtils.addSink(rf_a0, "rf_a0")

    val diff_te = Module(new DifftestTrapEvent)
    diff_te.io.clock    := clock
    diff_te.io.coreid   := 0.U
    diff_te.io.valid    := trap
    diff_te.io.cycleCnt := cycle_cnt
    diff_te.io.instrCnt := instr_cnt
    diff_te.io.hasWFI   := false.B
    diff_te.io.code     := rf_a0(2, 0)
    diff_te.io.pc       := commit_uop.pc

    val diff_ae = Module(new DifftestArchEvent)
    diff_ae.io.clock         := clock
    diff_ae.io.coreid        := 0.U
    diff_ae.io.intrNO        := 0.U
    diff_ae.io.cause         := 0.U
    diff_ae.io.exceptionPC   := 0.U
    diff_ae.io.exceptionInst := 0.U

    val diff_cs = Module(new DifftestCSRState)
    diff_cs.io.clock          := clock
    diff_cs.io.coreid         := 0.U
    diff_cs.io.priviledgeMode := 3.U
    diff_cs.io.mstatus        := 0.U
    diff_cs.io.sstatus        := 0.U
    diff_cs.io.mepc           := 0.U
    diff_cs.io.sepc           := 0.U
    diff_cs.io.mtval          := 0.U
    diff_cs.io.stval          := 0.U
    diff_cs.io.mtvec          := 0.U
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
