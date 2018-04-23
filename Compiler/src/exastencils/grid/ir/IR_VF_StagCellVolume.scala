package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_Domain
import exastencils.logger.Logger

/// IR_VF_StagCellVolume

object IR_VF_StagCellVolume {
  def find(level : Int, stagDim : Int) = IR_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellVolume", level).asInstanceOf[IR_VF_StagCellVolume]
  def access(level : Int, stagDim : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level, stagDim), index)
}

case class IR_VF_StagCellVolume(
    var level : Int,
    var domain : IR_Domain,
    var stagDim : Int
) extends IR_VirtualFieldWithScalar {

  override def name = s"vf_stag_${ stagDim }_cellVolume"
  override def knownAliases = ListBuffer(s"vf_stag_${ stagDim }_cellVol",
    s"vf_${ IR_Localization.dimToString(stagDim) }StagCellVolume", s"vf_${ IR_Localization.dimToString(stagDim) }StagCellVol")

  override def localization = IR_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def createDuplicate() = IR_VF_StagCellVolume(level, domain, stagDim)

  override def resolve(index : IR_ExpressionIndex) = {
    if (!Knowledge.grid_isStaggered) Logger.error("Trying to resolve a staggered quantity on a non-staggered grid; unsupported")

    if (Knowledge.grid_isAxisAligned) // includes uniform grids
      (0 until domain.numDims).map(dim => IR_VF_StagCellWidthPerDim.access(level, stagDim, dim, Duplicate(index)) : IR_Expression).reduce(_ * _)
    else
      Logger.error("Currently unsupported")
  }
}
