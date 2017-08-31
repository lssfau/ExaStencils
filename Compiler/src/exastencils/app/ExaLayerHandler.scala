package exastencils.app

import exastencils.app.ir._
import exastencils.app.l1._
import exastencils.app.l2._
import exastencils.app.l3._
import exastencils.app.l4._

/// ExaLayerHandler

object ExaLayerHandler {

  var l1_handler : L1_LayerHandler = L1_DefaultLayerHandler
  var l2_handler : L2_LayerHandler = L2_DefaultLayerHandler
  var l3_handler : L3_LayerHandler = L3_DefaultLayerHandler
  var l4_handler : L4_LayerHandler = L4_DefaultLayerHandler
  var ir_handler : IR_LayerHandler = IR_DefaultLayerHandler

  def allLayers = Array(l1_handler, l2_handler, l3_handler, l4_handler, ir_handler)

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