package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.domain.ir.IR_Domain

/// IR_VF_StagCellVolume

object IR_VF_StagCellVolume {
  def find(level : Int, stagDim : Int) = IR_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellVolume", level)
}

case class IR_VF_StagCellVolume(
    var level : Int,
    var domain : IR_Domain,
    var stagDim : Int
) extends IR_VirtualFieldWithScalar {

  override def name = s"vf_stag_${ stagDim }_cellVolume"
  override def knownAliases = ListBuffer("") // TODO
  override def localization = IR_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def resolve(index : IR_ExpressionIndex) = {
    (0 until domain.numDims).map(dim =>
      IR_VirtualFieldAccess(IR_VF_StagCellWidthPerDim.find(level, stagDim, dim), index) : IR_Expression
    ).reduce(_ * _)
  }
}
