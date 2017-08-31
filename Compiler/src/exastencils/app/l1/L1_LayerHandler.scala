package exastencils.app.l1

import exastencils.app.LayerHandler
import exastencils.base.ExaRootNode
import exastencils.base.l1.L1_Root
import exastencils.config._
import exastencils.datastructures.StrategyTimer
import exastencils.prettyprinting.Indenter

/// L1_LayerHandler

trait L1_LayerHandler extends LayerHandler

/// L1_DefaultLayerHandler

object L1_DefaultLayerHandler extends L1_LayerHandler {
  override def initialize() : Unit = {
    // activate default knowledge collections
    // nothing to do here yet
  }

  override def shutdown() : Unit = {
    // nothing to do here yet
  }

  override def print() : Unit = {
    if (Settings.getDebugL1file.nonEmpty) {
      val outFile = new java.io.FileWriter(Settings.getDebugL1file)
      outFile.write(Indenter.addIndentations(ExaRootNode.l1_root.prettyprint()))
      outFile.close()
    }
  }

  override def handle() : Unit = {
    if (Settings.timeStrategies) StrategyTimer.startTiming("Handling Layer 1")

    // add L1 code here
    if (Knowledge.experimental_layerExtension) {
      ExaRootNode.l1_root = L1_Root() // dummy node
      ExaRootNode.progressToL2()
    }

    if (Settings.timeStrategies) StrategyTimer.stopTiming("Handling Layer 1")
  }
}
