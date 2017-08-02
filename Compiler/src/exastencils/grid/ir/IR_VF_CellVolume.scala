package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.domain.ir.IR_Domain

/// IR_VF_CellVolume

object IR_VF_CellVolume {
  def find(level : Int) = IR_VirtualField.findVirtualField(s"vf_cellVolume", level)
}

case class IR_VF_CellVolume(
    var level : Int,
    var domain : IR_Domain
) extends IR_VirtualFieldWithScalar {

  override def name = "vf_cellVolume"
  override def knownAliases = ListBuffer("vf_cellVol")
  override def localization = IR_AtCellCenter
  override def resolutionPossible = true

  override def resolve(index : IR_ExpressionIndex) = {
    (0 until domain.numDims).map(dim =>
      IR_VirtualFieldAccess(IR_VF_CellWidthPerDim.find(level, dim), index) : IR_Expression
    ).reduce(_ * _)
  }
}
