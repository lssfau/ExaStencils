package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.domain.ir._

/// IR_VF_NodePositionAsVec

object IR_VF_NodePositionAsVec {
  def find(level : Int) = IR_VirtualField.findVirtualField(s"vf_nodePosition", level)
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

  override def resolve(index : IR_ExpressionIndex) = {
    index(dim) * IR_VirtualFieldAccess(IR_VF_CellWidthPerDim.find(level, dim), index) + IR_IV_FragmentPositionBegin(dim)
  }
}
