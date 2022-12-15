import chipsalliance.rocketchip.config._

case object EnableDifftest extends Field[Boolean]
case object Debug extends Field[Boolean]
case object ResetPC extends Field[BigInt]

class CoreConfig
    extends Config((site, here, up) => {
      case EnableDifftest => true
      case Debug          => false
      case ResetPC        => BigInt("80000000", 16)
    })

class CherrySpringsConfig extends Config(new CoreConfig)
