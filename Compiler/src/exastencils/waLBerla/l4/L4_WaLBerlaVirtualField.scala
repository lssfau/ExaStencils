package exastencils.waLBerla.l4

import exastencils.base.l4._
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.grid.l4._
import exastencils.logger.Logger
import exastencils.util.l4.L4_StackCollector

trait L4_WaLBerlaVirtualFieldWithVec extends L4_VirtualFieldWithVec {

  protected val vf : L4_VirtualFieldWithVec

  override def resolutionPossible : Boolean = false

  override def name = vf.name
  override def knownAliases = vf.knownAliases
  override def localization = vf.localization
  override def listPerDim = vf.listPerDim

  override def addAdditionalFieldsToKnowledge() = {}

}

trait L4_WaLBerlaVirtualFieldPerDim extends L4_VirtualFieldPerDim {

  protected val vf : L4_VirtualFieldPerDim

  override def resolutionPossible : Boolean = false

  override def name = vf.name
  override def knownAliases = vf.knownAliases
  override def localization = vf.localization

  override def resolve(index : L4_ExpressionIndex) : L4_Expression = Logger.error("Resolved unresolvable waLBerla vf")
}

object L4_WaLBerlaReplaceVirtualFieldAccesses extends DefaultStrategy("Replace vf accesses in wb scope with wb vf accesses") {

  var collector = new L4_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  def inWaLBerlaBlockLoop(collector : L4_StackCollector) =
    collector.stack.exists {
      case _ : L4_WaLBerlaLoopOverBlocks => true
      case _ : L4_WaLBerlaLoopOverField  => true
      case _                             => false
    }

  this += Transformation("..", {
    case _ @ L4_VirtualFieldAccess(L4_VF_CellCenterPerDim(lvl, domain, dim), index, arrayIndex) if inWaLBerlaBlockLoop(collector) =>
      new L4_VirtualFieldAccess(L4_WaLBerlaCellCenterPerDim(lvl, domain, dim), index, arrayIndex)
    case _ @ L4_VirtualFieldAccess(L4_VF_CellCenterAsVec(lvl, domain), index, arrayIndex) if inWaLBerlaBlockLoop(collector)       =>
      new L4_VirtualFieldAccess(L4_WaLBerlaCellCenterAsVec(lvl, domain), index, arrayIndex)

    case _ @ L4_VirtualFieldAccess(L4_VF_CellWidthPerDim(lvl, domain, dim), index, arrayIndex) if inWaLBerlaBlockLoop(collector) =>
      new L4_VirtualFieldAccess(L4_WaLBerlaCellWidthPerDim(lvl, domain, dim), index, arrayIndex)
    case _ @ L4_VirtualFieldAccess(L4_VF_CellWidthAsVec(lvl, domain), index, arrayIndex) if inWaLBerlaBlockLoop(collector)       =>
      new L4_VirtualFieldAccess(L4_WaLBerlaCellWidthAsVec(lvl, domain), index, arrayIndex)
  })
}