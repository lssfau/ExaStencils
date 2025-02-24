package exastencils.waLBerla.ir.grid

import scala.collection.mutable.ListBuffer

import exastencils.grid.ir._


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
