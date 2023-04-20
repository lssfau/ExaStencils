package exastencils.waLBerla.ir.grid

import scala.collection.mutable.ListBuffer

import exastencils.baseExt.ir.IR_LoopOverPoints
import exastencils.baseExt.ir.IR_LoopOverPointsInOneFragment
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.grid.ir._
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks
import exastencils.waLBerla.ir.field.IR_WaLBerlaField


trait IR_WaLBerlaVirtualFieldWithVec extends IR_VirtualFieldWithVec {
  protected val vf : IR_VirtualFieldWithVec

  override def resolutionPossible = true

  override def name = vf.name
  override def knownAliases = vf.knownAliases
  override def localization = vf.localization
  override def listPerDim = vf.listPerDim

  override def generateInitCode() = ListBuffer()
  override def generateInitCodeDependsOn() = ListBuffer()
}

trait IR_WaLBerlaVirtualFieldPerDim extends IR_VirtualFieldPerDim {
  protected val vf : IR_VirtualFieldPerDim

  override def resolutionPossible = true

  override def name = vf.name
  override def knownAliases = vf.knownAliases
  override def localization = vf.localization
}


object IR_WaLBerlaReplaceVirtualFieldAccesses extends DefaultStrategy("Replace vf accesses with waLBerla function calls") {
  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  def inWaLBerlaBlockLoop(collector : IR_StackCollector) = {
    collector.stack.exists {
      case _ : IR_WaLBerlaLoopOverBlocks                                                      => true
      case loop : IR_LoopOverPoints if loop.field.isInstanceOf[IR_WaLBerlaField]              => true
      case loop : IR_LoopOverPointsInOneFragment if loop.field.isInstanceOf[IR_WaLBerlaField] => true
      case _                                                                                  => false
    }
  }

  // replace vf accesses
  this += Transformation("Replace", {
    case _ @ IR_VirtualFieldAccess(IR_VF_CellCenterPerDim(lvl, domain, dim), idx, fragIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_VirtualFieldAccess(IR_WaLBerlaCellCenterPerDim(lvl, domain, dim), idx, fragIdx)
    case _ @ IR_VirtualFieldAccess(IR_VF_CellCenterAsVec(lvl, domain), idx, fragIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_VirtualFieldAccess(IR_WaLBerlaCellCenterAsVec(lvl, domain), idx, fragIdx)

    case _ @ IR_VirtualFieldAccess(IR_VF_CellWidthPerDim(lvl, domain, dim), idx, fragIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_VirtualFieldAccess(IR_WaLBerlaCellWidthPerDim(lvl, domain, dim), idx, fragIdx)
    case _ @ IR_VirtualFieldAccess(IR_VF_CellWidthAsVec(lvl, domain), idx, fragIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_VirtualFieldAccess(IR_WaLBerlaCellWidthAsVec(lvl, domain), idx, fragIdx)

    // boundary positions snap from cell center to node position -> introduce waLBerla node positions
    case _ @ IR_VirtualFieldAccess(IR_VF_NodePositionPerDim(lvl, domain, dim), idx, fragIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_VirtualFieldAccess(IR_WaLBerlaNodePositionPerDim(lvl, domain, dim), idx, fragIdx)
    case _ @ IR_VirtualFieldAccess(IR_VF_NodePositionAsVec(lvl, domain), idx, fragIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_VirtualFieldAccess(IR_WaLBerlaNodePositionAsVec(lvl, domain), idx, fragIdx)
  })
}
