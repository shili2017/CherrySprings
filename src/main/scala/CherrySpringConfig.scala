import chipsalliance.rocketchip.config._
import chisel3._

case object EnableDifftest extends Field[Boolean]
case object Debug extends Field[Boolean]
case object ResetPC extends Field[BigInt]
case object XLen extends Field[Int]

class CoreConfig
    extends Config((site, here, up) => {
      case EnableDifftest => true
      case ResetPC        => BigInt("80000000", 16)
    })

class DebugConfig
    extends Config((site, here, up) => {
      case Debug => false
    })

class CherrySpringsConfig extends Config(new CoreConfig ++ new DebugConfig)

trait HasCherrySpringsParameters {
  implicit val p: Parameters
  def enableDifftest: Boolean = p(EnableDifftest)
  def resetPC:        BigInt  = p(ResetPC)
  def debug:          Boolean = p(Debug)
  def xLen:           Int     = 64
}

abstract class CherrySpringsModule(implicit val p: Parameters) extends Module with HasCherrySpringsParameters

class ParameterizedBundle(implicit p: Parameters) extends Bundle

abstract class CherrySpringsBundle(implicit val p: Parameters)
    extends ParameterizedBundle()(p)
    with HasCherrySpringsParameters
