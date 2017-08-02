package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.domain.l4.L4_Domain
import exastencils.grid.ir.IR_VF_StagCellVolume

/// L4_VF_StagCellVolume

object L4_VF_StagCellVolume {
  def find(level : Int, stagDim : Int) = L4_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellVolume", level)
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
    (0 until domain.numDims).map(dim =>
      L4_VirtualFieldAccess(L4_VF_StagCellWidthPerDim.find(level, stagDim, dim), index) : L4_Expression
    ).reduce(_ * _)
  }

  override def progressImpl() = IR_VF_StagCellVolume(level, domain.getProgressedObj(), stagDim)
}
