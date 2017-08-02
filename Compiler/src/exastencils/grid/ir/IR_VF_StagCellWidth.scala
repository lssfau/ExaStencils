package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.domain.ir.IR_Domain

/// IR_VF_StagCellWidthAsVec

object IR_VF_StagCellWidthAsVec {
  def find(level : Int, stagDim : Int) = IR_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellWidth", level)
}

case class IR_VF_StagCellWidthAsVec(
    var level : Int,
    var domain : IR_Domain,
    var stagDim : Int
) extends IR_VirtualFieldWithVec {

  override def name = s"vf_stag_${ stagDim }_cellWidth"
  override def knownAliases = ListBuffer("") // TODO
  override def localization = IR_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(IR_VF_StagCellWidthPerDim.find(level, stagDim, _)).to[ListBuffer]
}

/// IR_VF_StagCellWidthPerDim

object IR_VF_StagCellWidthPerDim {
  def find(level : Int, stagDim : Int, dim : Int) = IR_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellWidth_$dim", level)
}

case class IR_VF_StagCellWidthPerDim(
    var level : Int,
    var domain : IR_Domain,
    var stagDim : Int,
    var dim : Int
) extends IR_VirtualFieldPerDim {

  override def name = s"vf_stag_${ stagDim }_cellWidth_$dim"
  override def knownAliases = ListBuffer("") // TODO
  override def localization = IR_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def resolve(index : IR_ExpressionIndex) = {
    if (dim == stagDim)
      0.5 * (
        IR_VirtualFieldAccess(IR_VF_CellWidthPerDim.find(level, dim), IR_GridUtil.offsetIndex(index, -1, dim))
          + IR_VirtualFieldAccess(IR_VF_CellWidthPerDim.find(level, dim), index))
    else
      IR_VirtualFieldAccess(IR_VF_CellWidthPerDim.find(level, dim), index)
  }
}
