package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.config.Knowledge
import exastencils.domain.l4.L4_Domain
import exastencils.logger.Logger
import exastencils.grid.ir.IR_VF_StagCellVolume

/// L4_VF_StagCellVolume

object L4_VF_StagCellVolume {
  def find(level : Int, stagDim : Int) = L4_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellVolume", level)
  def access(level : Int, stagDim : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level, stagDim), index)
}

case class L4_VF_StagCellVolume(
    var level : Int,
    var domain : L4_Domain,
    var stagDim : Int
) extends L4_VirtualFieldWithScalar {

  override def name = s"vf_stag_${ stagDim }_cellVolume"
  override def knownAliases = ListBuffer("") // TODO
  override def localization = L4_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def resolve(index : L4_ExpressionIndex) = {
    if (!Knowledge.grid_isStaggered) Logger.error("Trying to resolve a staggered quantity on a non-staggered grid; unsupported")

    if (Knowledge.grid_isAxisAligned) // includes uniform grids
      (0 until domain.numDims).map(dim => L4_VF_StagCellWidthPerDim.access(level, stagDim, dim, index) : L4_Expression).reduce(_ * _)
    else
      Logger.error("Currently unsupported")
  }

  override def progressImpl() = IR_VF_StagCellVolume(level, domain.getProgressedObj(), stagDim)
}
