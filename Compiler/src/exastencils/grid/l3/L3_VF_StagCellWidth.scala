package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.config.Knowledge
import exastencils.domain.l3.L3_Domain
import exastencils.grid.l4._
import exastencils.logger.Logger

/// L3_VF_StagCellWidthAsVec

object L3_VF_StagCellWidthAsVec {
  def find(level : Int, stagDim : Int) = L3_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellWidth", level)
  def access(level : Int, stagDim : Int, index : L3_ExpressionIndex) = L3_VirtualFieldAccess(find(level, stagDim), index)
}

case class L3_VF_StagCellWidthAsVec(
    var level : Int,
    var domain : L3_Domain,
    var stagDim : Int
) extends L3_VirtualFieldWithVec {

  override def name = s"vf_stag_${ stagDim }_cellWidth"
  override def knownAliases = ListBuffer(s"vf_${ L3_Localization.dimToString(stagDim) }StagCVWidth", s"vf_${ L3_Localization.dimToString(stagDim) }StagCellWidth")
  override def localization = L3_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L3_VF_StagCellWidthPerDim.find(level, stagDim, _)).to[ListBuffer]

  override def progressImpl() = L4_VF_StagCellWidthAsVec(level, domain.getProgressedObj(), stagDim)
}

/// L3_VF_StagCellWidthPerDim

object L3_VF_StagCellWidthPerDim {
  def find(level : Int, stagDim : Int, dim : Int) = L3_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellWidth_$dim", level)
  def access(level : Int, stagDim : Int, dim : Int, index : L3_ExpressionIndex) = L3_VirtualFieldAccess(find(level, stagDim, dim), index)
}

case class L3_VF_StagCellWidthPerDim(
    var level : Int,
    var domain : L3_Domain,
    var stagDim : Int,
    var dim : Int
) extends L3_VirtualFieldPerDim {

  override def name = s"vf_stag_${ stagDim }_cellWidth_$dim"
  override def knownAliases = {
    def aliases = ListBuffer(
      s"vf_${ L3_Localization.dimToString(stagDim) }StagCVWidth_${ L3_Localization.dimToString(dim) }",
      s"vf_${ L3_Localization.dimToString(stagDim) }StagCellWidth_$dim}",
      s"vf_stag_${ stagDim }_cellWidth_${ L3_Localization.dimToString(dim) }")
    if (dim == stagDim) aliases += s"vf_stagCVWidth_${ L3_Localization.dimToString(dim) }" // backwards compatibility
    aliases
  }
  override def localization = L3_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def resolve(index : L3_ExpressionIndex) = {
    if (!Knowledge.grid_isStaggered) Logger.error("Trying to resolve a staggered quantity on a non-staggered grid; unsupported")

    if (Knowledge.grid_isAxisAligned) { // includes uniform grids
      if (dim == stagDim) // half of this cell and half of the left neighbor cell
        0.5 * (L3_VF_CellWidthPerDim.access(level, dim, L3_GridUtil.offsetIndex(index, -1, dim))
          + L3_VF_CellWidthPerDim.access(level, dim, index))
      else // just the un-staggered cell width
        L3_VF_CellWidthPerDim.access(level, dim, index)

    } else {
      Logger.error("Currently unsupported")
    }
  }

  override def progressImpl() = L4_VF_StagCellWidthPerDim(level, domain.getProgressedObj(), stagDim, dim)
}
