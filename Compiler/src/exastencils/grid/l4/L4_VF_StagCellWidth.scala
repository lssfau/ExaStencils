package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.boundary.l4.L4_NoBC
import exastencils.config.Knowledge
import exastencils.domain.l4.L4_Domain
import exastencils.field.l4._
import exastencils.grid.ir._
import exastencils.logger.Logger

/// L4_VF_StagCellWidthAsVec

object L4_VF_StagCellWidthAsVec {
  def find(level : Int, stagDim : Int) = L4_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellWidth", level)
  def access(level : Int, stagDim : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level, stagDim), index)
}

case class L4_VF_StagCellWidthAsVec(
    var level : Int,
    var domain : L4_Domain,
    var stagDim : Int
) extends L4_VirtualFieldWithVec {

  override def name = s"vf_stag_${ stagDim }_cellWidth"
  override def knownAliases = ListBuffer(s"vf_${ L4_Localization.dimToString(stagDim) }StagCVWidth", s"vf_${ L4_Localization.dimToString(stagDim) }StagCellWidth")
  override def localization = L4_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L4_VF_StagCellWidthPerDim.find(level, stagDim, _)).to[ListBuffer]

  override def progressImpl() = IR_VF_StagCellWidthAsVec(level, domain.getProgressedObj(), stagDim)
}

/// L4_VF_StagCellWidthPerDim

object L4_VF_StagCellWidthPerDim {
  def find(level : Int, stagDim : Int, dim : Int) = L4_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellWidth_$dim", level)
  def access(level : Int, stagDim : Int, dim : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level, stagDim, dim), index)
}

case class L4_VF_StagCellWidthPerDim(
    var level : Int,
    var domain : L4_Domain,
    var stagDim : Int,
    var dim : Int
) extends L4_VirtualFieldPerDim {

  override def name = s"vf_stag_${ stagDim }_cellWidth_$dim"
  override def knownAliases = {
    var aliases = ListBuffer(
      s"vf_${ L4_Localization.dimToString(stagDim) }StagCVWidth_${ L4_Localization.dimToString(dim) }",
      s"vf_${ L4_Localization.dimToString(stagDim) }StagCellWidth_$dim}",
      s"vf_stag_${ stagDim }_cellWidth_${ L4_Localization.dimToString(dim) }")
    if (dim == stagDim) aliases += s"vf_stagCVWidth_${ L4_Localization.dimToString(dim) }" // backwards compatibility
    aliases
  }
  override def localization = L4_AtFaceCenter(stagDim)
  override def resolutionPossible = Knowledge.grid_isUniform || (Knowledge.grid_isAxisAligned && !Knowledge.grid_halveStagBoundaryVolumes)

  override def addAdditionalFieldsToKnowledge() = {
    if (Knowledge.grid_isAxisAligned && !Knowledge.grid_isUniform && Knowledge.grid_isStaggered) {
      // TODO: Knowledge.grid_halveStagBoundaryVolumes

      def zeroIndex = L4_ConstIndex(Array.fill(domain.numDims)(0))
      def oneIndex = L4_ConstIndex(Array.fill(domain.numDims)(1))

      val layout = L4_FieldLayout(
        s"vf_stagCellWidthPerDim_${ dim }_layout", level, numDims,
        L4_RealDatatype, L4_HACK_OtherLocalization("Edge_Node"),
        L4_GridUtil.offsetIndex(zeroIndex, 2, dim), communicatesGhosts = true,
        L4_GridUtil.offsetIndex(zeroIndex, 1, dim), communicatesDuplicated = true,
        L4_GridUtil.offsetIndex(oneIndex, ((1 << level) * Knowledge.domain_fragmentLengthAsVec(dim) - 1) - 1, dim))

      val fieldIndex = L4_FieldDecl.runningIndex
      L4_FieldDecl.runningIndex += 1

      val field = L4_Field(s"stag_cv_width_${ IR_Localization.dimToString(dim) }", level, fieldIndex, domain, layout, 1, L4_NoBC)

      L4_FieldLayoutCollection.add(layout)
      L4_FieldCollection.add(field)
    }
  }

  override def resolve(index : L4_ExpressionIndex) = {
    if (!Knowledge.grid_isStaggered) Logger.error("Trying to resolve a staggered quantity on a non-staggered grid; unsupported")

    if (Knowledge.grid_isUniform || (Knowledge.grid_isAxisAligned && !Knowledge.grid_halveStagBoundaryVolumes)) {
      // construct data on the fly if the grid is either uniform or non-uniform but without halved control volumes at the boundaries

      if (dim == stagDim) // half of this cell and half of the left neighbor cell
        0.5 * (L4_VF_CellWidthPerDim.access(level, dim, L4_GridUtil.offsetIndex(index, -1, dim))
          + L4_VF_CellWidthPerDim.access(level, dim, index))
      else // just the un-staggered cell width
        L4_VF_CellWidthPerDim.access(level, dim, index)

    } else {
      Logger.error("Currently unsupported")
    }
  }

  override def progressImpl() = IR_VF_StagCellWidthPerDim(level, domain.getProgressedObj(), stagDim, dim)
}
