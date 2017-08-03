package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_Domain
import exastencils.logger.Logger

/// IR_VF_StagCellWidthAsVec

object IR_VF_StagCellWidthAsVec {
  def find(level : Int, stagDim : Int) = IR_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellWidth", level)
  def access(level : Int, stagDim : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level, stagDim), index)
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
  def access(level : Int, stagDim : Int, dim : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level, stagDim, dim), index)
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
    if (!Knowledge.grid_isStaggered) Logger.error("Trying to resolve a staggered quantity on a non-staggered grid; unsupported")

    if (Knowledge.grid_isAxisAligned) { // includes uniform grids
      if (dim == stagDim) // half of this cell and half of the left neighbor cell
        0.5 * (IR_VF_CellWidthPerDim.access(level, dim, IR_GridUtil.offsetIndex(index, -1, dim))
          + IR_VF_CellWidthPerDim.access(level, dim, index))
      else // just the un-staggered cell width
        IR_VF_CellWidthPerDim.access(level, dim, index)

    } else {
      Logger.error("Currently unsupported")
    }
  }
}
