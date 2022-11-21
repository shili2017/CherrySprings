import chisel3._
import chisel3.util._

class CachePortReq extends Bundle {
  val addr  = Output(UInt(32.W))
  val wdata = Output(UInt(32.W))
  val wmask = Output(UInt(4.W))
  val wen   = Output(Bool())
}

class CachePortResp extends Bundle {
  val rdata = Output(UInt(32.W))
}

class CachePortIO extends Bundle {
  val req  = Decoupled(new CachePortReq)
  val resp = Flipped(Decoupled(new CachePortResp))
}
