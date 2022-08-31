package cherryriver

import chipsalliance.rocketchip.config.Parameters
import chisel3.stage.ChiselStage
import freechips.rocketchip.diplomacy._

object Elaborate extends App {
  (new ChiselStage).execute(args, Seq(chisel3.stage.ChiselGeneratorAnnotation(
    () => LazyModule(new AdderTestHarness()(Parameters.empty)).module))
  )
}