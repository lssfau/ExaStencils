package exastencils.waLBerla.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.domain.ir._
import exastencils.grid.ir._
import exastencils.knowledge.ir.IR_KnowledgeObject


case class IR_WaLBerlaCellCenterAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_WaLBerlaVirtualFieldWithVec {

  override protected val vf = IR_VF_CellCenterAsVec(level, domain)

  override def createDuplicate() = IR_WaLBerlaCellCenterAsVec(level, domain)
}

case class IR_WaLBerlaCellCenterPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_WaLBerlaVirtualFieldPerDim {

  override protected val vf = IR_VF_CellCenterPerDim(level, domain, dim)

  override def createDuplicate() : IR_KnowledgeObject = IR_WaLBerlaCellCenterPerDim(level, domain, dim)

  def aabbDatatype = IR_SpecialDatatype("math::AABB")

  def getCellAABB(idx : IR_ExpressionIndex) =
    IR_MemberFunctionCallArrow(IR_WaLBerlaUtil.getBlockForest, "getBlockLocalCellAABB", aabbDatatype,
      IR_DerefAccess(IR_WaLBerlaLoopOverBlocks.defIt), IR_FunctionCall(IR_ExternalFunctionReference("Cell"), idx.indices.padTo(3, 0 : IR_Expression) : _*))

  override def resolve(index : IR_ExpressionIndex) = IR_ArrayAccess(IR_MemberFunctionCall(getCellAABB(index), "center"), dim)
}
