package exastencils.waLBerla.ir

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.domain.ir.IR_Domain
import exastencils.grid.ir.IR_VF_CellWidthAsVec
import exastencils.grid.ir.IR_VF_CellWidthPerDim

case class IR_WaLBerlaCellWidthAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_WaLBerlaVirtualFieldWithVec {

  override protected val vf = IR_VF_CellWidthAsVec(level, domain)

  override def createDuplicate() = IR_WaLBerlaCellWidthAsVec(level, domain)
}

case class IR_WaLBerlaCellWidthPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_WaLBerlaVirtualFieldPerDim {

  override protected val vf = IR_VF_CellWidthPerDim(level, domain, dim)

  def aabbDatatype = IR_SpecialDatatype("math::AABB")

  def getCellAABB(idx : IR_ExpressionIndex) =
    IR_MemberFunctionCallArrow(IR_WaLBerlaUtil.getBlockForest, "getCellAABB", aabbDatatype,
      IR_FunctionCall(IR_ExternalFunctionReference("Cell"), idx.indices.padTo(3, 0 : IR_Expression) : _*))

  override def createDuplicate() = IR_WaLBerlaCellWidthPerDim(level, domain, dim)

  override def resolve(index : IR_ExpressionIndex) = IR_MemberFunctionCall(getCellAABB(index), "size", dim)
}