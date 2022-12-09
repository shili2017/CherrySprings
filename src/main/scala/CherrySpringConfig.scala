import chipsalliance.rocketchip.config._

case object EnableDifftest extends Field[Boolean]
case object Debug extends Field[Boolean]

class CoreConfig
    extends Config((site, here, up) => {
      case EnableDifftest => true
      case Debug          => false
    })

class CherrySpringsConfig extends Config(new CoreConfig)
