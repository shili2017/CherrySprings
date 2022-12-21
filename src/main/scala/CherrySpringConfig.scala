import chipsalliance.rocketchip.config._
import chisel3._
import freechips.rocketchip.devices.debug.Debug

case object EnableDifftest extends Field[Boolean]
case object ResetPC extends Field[BigInt]
case object HartID extends Field[Int]
case object DebugCommit extends Field[Boolean]
case object DebugLoadStore extends Field[Boolean]
case object DebugAXI4 extends Field[Boolean]

class CoreConfig
    extends Config((site, here, up) => {
      case EnableDifftest => true
      case ResetPC        => BigInt("80000000", 16)
      case HartID         => 0
    })

class DebugConfig
    extends Config((site, here, up) => {
      case DebugCommit    => false
      case DebugLoadStore => false
      case DebugAXI4      => true
    })

class CherrySpringsConfig extends Config(new CoreConfig ++ new DebugConfig)

trait HasCherrySpringsParameters {
  implicit val p: Parameters
  def enableDifftest: Boolean = p(EnableDifftest)
  def resetPC:        BigInt  = p(ResetPC)
  def hartID:         Int     = p(HartID)
  def debugCommit:    Boolean = p(DebugCommit)
  def debugLoadStore: Boolean = p(DebugLoadStore)
  def debugAXI4:      Boolean = p(DebugAXI4)
  def xLen:           Int     = 64
}

abstract class CherrySpringsModule(implicit val p: Parameters) extends Module with HasCherrySpringsParameters

class ParameterizedBundle(implicit p: Parameters) extends Bundle

abstract class CherrySpringsBundle(implicit val p: Parameters)
    extends ParameterizedBundle()(p)
    with HasCherrySpringsParameters
