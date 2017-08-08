package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.deprecated.ir._
import exastencils.domain.ir._
import exastencils.field.ir._

/// IR_VF_NodePositionAsVec

object IR_VF_NodePositionAsVec {
  def find(level : Int) = IR_VirtualField.findVirtualField(s"vf_nodePosition", level)
  def access(level : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level), index)
}

case class IR_VF_NodePositionAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_VirtualFieldWithVec {

  override def name = "vf_nodePosition"
  override def knownAliases = ListBuffer("vf_nodePositionAsVec", "vf_nodePosAsVec", "vf_nodePos")
  override def localization = IR_AtNode
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(IR_VF_NodePositionPerDim.find(level, _)).to[ListBuffer]
}

/// IR_VF_NodePositionPerDim

object IR_VF_NodePositionPerDim {
  def find(level : Int, dim : Int) = IR_VirtualField.findVirtualField(s"vf_nodePosition_$dim", level)
  def access(level : Int, dim : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level, dim), index)
}

case class IR_VF_NodePositionPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_VirtualFieldPerDim {

  override def name = s"vf_nodePosition_$dim"
  override def knownAliases = ListBuffer(s"vf_nodePosition_${ IR_Localization.dimToString(dim) }", s"vf_nodePos_$dim", s"vf_nodePos_${ IR_Localization.dimToString(dim) }")
  override def localization = IR_AtNode
  override def resolutionPossible = true

  def associatedField = IR_FieldCollection.getByIdentifier(s"node_pos_${ IR_Localization.dimToString(dim) }", level).get

  override def resolve(index : IR_ExpressionIndex) : IR_Expression = {
    if (Knowledge.grid_isUniform)
      index(dim) * IR_VF_CellWidthPerDim.access(level, dim, Duplicate(index)) + IR_IV_FragmentPositionBegin(dim)
    else if (Knowledge.grid_isAxisAligned)
      IR_FieldAccess(IR_FieldSelection(associatedField, level, 0), IR_GridUtil.projectIdx(index, dim))
    else
      IR_FieldAccess(IR_FieldSelection(associatedField, level, 0), index)
  }
}
