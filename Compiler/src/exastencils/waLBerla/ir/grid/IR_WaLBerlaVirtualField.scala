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
