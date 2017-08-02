package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_Domain

/// IR_VF_CellWidthAsVec

object IR_VF_CellWidthAsVec {
  def find(level : Int) = IR_VirtualField.findVirtualField(s"vf_cellWidth", level)
}

case class IR_VF_CellWidthAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_VirtualFieldWithVec {

  override def name = "vf_cellWidth"
  override def knownAliases = ListBuffer("vf_cellWidthAsVec", "vf_gridWidthAsVec", "vf_gridWidth")
  override def localization = IR_AtCellCenter
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(IR_VF_CellWidthPerDim.find(level, _)).to[ListBuffer]
}

/// IR_VF_CellWidthPerDim

object IR_VF_CellWidthPerDim {
  def find(level : Int, dim : Int) = IR_VirtualField.findVirtualField(s"vf_cellWidth_$dim", level)
}

case class IR_VF_CellWidthPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_VirtualFieldPerDim {

  override def name = s"vf_cellWidth_$dim"
  override def knownAliases = ListBuffer(s"vf_cellWidth_${ IR_Localization.dimToString(dim) }", s"vf_gridWidth_$dim", s"vf_gridWidth_${ IR_Localization.dimToString(dim) }")
  override def localization = IR_AtCellCenter
  override def resolutionPossible = true

  override def resolve(index : IR_ExpressionIndex) = {
    val levelIndex = level - Knowledge.minLevel
    dim match {
      case 0 => Knowledge.discr_hx(levelIndex)
      case 1 => Knowledge.discr_hy(levelIndex)
      case 2 => Knowledge.discr_hz(levelIndex)
    }
  }
}
