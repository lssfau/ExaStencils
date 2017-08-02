package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.domain.l3.L3_Domain
import exastencils.grid.l4._

/// L3_VF_StagCellWidthAsVec

object L3_VF_StagCellWidthAsVec {
  def find(level : Int, stagDim : Int) = L3_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellWidth", level)
}

case class L3_VF_StagCellWidthAsVec(
    var level : Int,
    var domain : L3_Domain,
    var stagDim : Int
) extends L3_VirtualFieldWithVec {

  override def name = s"vf_stag_${ stagDim }_cellWidth"
  override def knownAliases = ListBuffer("") // TODO
  override def localization = L3_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L3_VF_StagCellWidthPerDim.find(level, stagDim, _)).to[ListBuffer]

  override def progressImpl() = L4_VF_StagCellWidthAsVec(level, domain.getProgressedObj(), stagDim)
}

/// L3_VF_StagCellWidthPerDim

object L3_VF_StagCellWidthPerDim {
  def find(level : Int, stagDim : Int, dim : Int) = L3_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellWidth_$dim", level)
}

case class L3_VF_StagCellWidthPerDim(
    var level : Int,
    var domain : L3_Domain,
    var stagDim : Int,
    var dim : Int
) extends L3_VirtualFieldPerDim {

  override def name = s"vf_stag_${ stagDim }_cellWidth_$dim"
  override def knownAliases = ListBuffer("") // TODO
  override def localization = L3_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def resolve(index : L3_ExpressionIndex) = {
    if (dim == stagDim)
      0.5 * (
        L3_VirtualFieldAccess(L3_VF_CellWidthPerDim.find(level, dim), L3_GridUtil.offsetIndex(index, -1, dim))
          + L3_VirtualFieldAccess(L3_VF_CellWidthPerDim.find(level, dim), index))
    else
      L3_VirtualFieldAccess(L3_VF_CellWidthPerDim.find(level, dim), index)
  }

  override def progressImpl() = L4_VF_StagCellWidthPerDim(level, domain.getProgressedObj(), stagDim, dim)
}
