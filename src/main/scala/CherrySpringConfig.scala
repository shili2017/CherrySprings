import chipsalliance.rocketchip.config._

case object EnableDifftest extends Field[Boolean]

class CoreConfig
    extends Config((site, here, up) => {
      case EnableDifftest => true
    })

class CherrySpringsConfig extends Config(new CoreConfig)
