package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_Domain
import exastencils.field.ir._
import exastencils.logger.Logger

/// IR_VF_StagCellWidthAsVec

object IR_VF_StagCellWidthAsVec {
  def find(level : Int, stagDim : Int) = IR_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellWidth", level).asInstanceOf[IR_VF_StagCellWidthAsVec]
  def access(level : Int, stagDim : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level, stagDim), index)
}

case class IR_VF_StagCellWidthAsVec(
    var level : Int,
    var domain : IR_Domain,
    var stagDim : Int
) extends IR_VirtualFieldWithVec {

  override def name = s"vf_stag_${ stagDim }_cellWidth"
  override def knownAliases = ListBuffer(s"vf_${ IR_Localization.dimToString(stagDim) }StagCVWidth", s"vf_${ IR_Localization.dimToString(stagDim) }StagCellWidth")
  override def localization = IR_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def createDuplicate() = IR_VF_StagCellWidthAsVec(level, domain, stagDim)

  override def listPerDim = (0 until numDims).map(IR_VF_StagCellWidthPerDim.find(level, stagDim, _) : IR_VirtualField).to[ListBuffer]
}

/// IR_VF_StagCellWidthPerDim

object IR_VF_StagCellWidthPerDim {
  def find(level : Int, stagDim : Int, dim : Int) = IR_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellWidth_$dim", level).asInstanceOf[IR_VF_StagCellWidthPerDim]
  def access(level : Int, stagDim : Int, dim : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level, stagDim, dim), index)
}

case class IR_VF_StagCellWidthPerDim(
    var level : Int,
    var domain : IR_Domain,
    var stagDim : Int,
    var dim : Int
) extends IR_VirtualFieldPerDim {

  override def name = s"vf_stag_${ stagDim }_cellWidth_$dim"
  override def knownAliases = {
    var aliases = ListBuffer(
      s"vf_${ IR_Localization.dimToString(stagDim) }StagCVWidth_${ IR_Localization.dimToString(dim) }",
      s"vf_${ IR_Localization.dimToString(stagDim) }StagCellWidth_$dim}",
      s"vf_stag_${ stagDim }_cellWidth_${ IR_Localization.dimToString(dim) }")
    if (dim == stagDim) aliases += s"vf_stagCVWidth_${ IR_Localization.dimToString(dim) }" // backwards compatibility
    aliases
  }
  override def localization = IR_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def createDuplicate() = IR_VF_StagCellWidthPerDim(level, domain, stagDim, dim)

  def associatedField = {
    if (Knowledge.grid_isAxisAligned && !Knowledge.grid_isUniform && (dim == stagDim) && stagDim == dim)
      IR_FieldCollection.getByIdentifier(name, level).get
    else
      Logger.error("Trying to access associated field for IR_VF_StagCellWidthPerDim; not found")
  }

  override def resolve(index : IR_ExpressionIndex) = {
    if (!Knowledge.grid_isStaggered) Logger.error("Trying to resolve a staggered quantity on a non-staggered grid; unsupported")

    if (Knowledge.grid_isUniform || (Knowledge.grid_isAxisAligned && !Knowledge.grid_halveStagBoundaryVolumes)) {
      // construct data on the fly if the grid is either uniform or non-uniform but without halved control volumes at the boundaries

      if (dim == stagDim) // half of this cell and half of the left neighbor cell
        0.5 * (IR_VF_CellWidthPerDim.access(level, dim, IR_GridUtil.offsetIndex(index, -1, dim))
          + IR_VF_CellWidthPerDim.access(level, dim, index))
      else // just the un-staggered cell width
        IR_VF_CellWidthPerDim.access(level, dim, index)

    } else if (Knowledge.grid_isAxisAligned) {
      // special handling is required at the boundaries since grid_halveStagBoundaryVolumes is true
      // -> read widths from field in stagger dimensions

      if (dim == stagDim) // read from field
        IR_FieldAccess(associatedField, 0, IR_GridUtil.projectIdx(index, dim))
      else // just the un-staggered cell width
        IR_VF_CellWidthPerDim.access(level, dim, index)

    } else {
      Logger.error("Currently unsupported")
    }
  }

  override def generateInitCode() = {
    if (!Knowledge.grid_isStaggered) Logger.error("Trying to generate init code for a staggered quantity on a non-staggered grid; unsupported")

    val stmts = ListBuffer[IR_Statement]()

    if (Knowledge.grid_isAxisAligned && !Knowledge.grid_isUniform && Knowledge.grid_halveStagBoundaryVolumes && stagDim == dim) {
      stmts ++= IR_SetupStagCellWidth.for_AA(level, dim)
    }

    stmts
  }

  override def generateInitCodeDependsOn() = {
    if (Knowledge.grid_isAxisAligned && !Knowledge.grid_isUniform && Knowledge.grid_halveStagBoundaryVolumes && stagDim == dim)
      ListBuffer(IR_VF_NodePositionPerDim.find(level, dim))
    else
      ListBuffer()
  }
}
