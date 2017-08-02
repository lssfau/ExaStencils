package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.domain.l2.L2_Domain
import exastencils.grid.l3._

/// L2_VF_StagCellWidthAsVec

case class L2_VF_StagCellWidthAsVec(
    var level : Int,
    var domain : L2_Domain,
    var stagDim : Int
) extends L2_VirtualFieldWithVec {

  override def name = s"vf_stag_${ stagDim }_cellWidth"
  override def knownAliases = ListBuffer("") // TODO
  override def localization = L2_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L2_VF_StagCellWidthPerDim(level, domain, stagDim, _) : L2_VirtualFieldPerDim).to[ListBuffer]

  override def progressImpl() = L3_VF_StagCellWidthAsVec(level, domain.getProgressedObj(), stagDim)
}

/// L2_VF_StagCellWidthPerDim

case class L2_VF_StagCellWidthPerDim(
    var level : Int,
    var domain : L2_Domain,
    var stagDim : Int,
    var dim : Int
) extends L2_VirtualFieldPerDim {

  override def name = s"vf_stag_${ stagDim }_cellWidth_$dim"
  override def knownAliases = ListBuffer("") // TODO
  override def localization = L2_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def resolve(index : L2_ExpressionIndex) = {
    if (dim == stagDim)
      0.5 * (
        L2_VirtualFieldAccess(L2_VF_CellWidthPerDim(level, domain, dim), L2_GridUtil.offsetIndex(index, -1, dim))
          + L2_VirtualFieldAccess(L2_VF_CellWidthPerDim(level, domain, dim), index))
    else
      L2_VirtualFieldAccess(L2_VF_CellWidthPerDim(level, domain, dim), index)
  }

  override def progressImpl() = L3_VF_StagCellWidthPerDim(level, domain.getProgressedObj(), stagDim, dim)
}
