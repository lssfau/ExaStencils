package exastencils.app

import exastencils.app.ir._
import exastencils.app.l1._
import exastencils.app.l2._
import exastencils.app.l3._
import exastencils.app.l4._
import exastencils.config.Settings

/// ExaLayerHandler

object ExaLayerHandler {
  var l1_handler : L1_LayerHandler = L1_DefaultLayerHandler
  var l2_handler : L2_LayerHandler = L2_DefaultLayerHandler
  var l3_handler : L3_LayerHandler = L3_DefaultLayerHandler
  var l4_handler : L4_LayerHandler = L4_DefaultLayerHandler
  var ir_handler : IR_LayerHandler = IR_DefaultLayerHandler

  def allLayers = Array(l1_handler, l2_handler, l3_handler, l4_handler, ir_handler)

  def maskUnusedLayers() : Unit = {
    if (Settings.minLayerFileProvided > 1) l1_handler = L1_DummyLayerHandler
    if (Settings.minLayerFileProvided > 2) l2_handler = L2_DummyLayerHandler
    if (Settings.minLayerFileProvided > 3) l3_handler = L3_DummyLayerHandler
    if (Settings.minLayerFileProvided > 4) l4_handler = L4_DummyLayerHandler
    if (Settings.minLayerFileProvided > 5) ir_handler = IR_DummyLayerHandler
  }

  def initializeAllLayers() : Unit = {
    allLayers.foreach(_.initialize())
  }

  def handleAllLayers() : Unit = {
    allLayers.foreach(_.handle())
  }

  def shutdownAllLayers() : Unit = {
    allLayers.foreach(_.shutdown())
  }
}