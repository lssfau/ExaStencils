package exastencils.waLBerla.ir.grid

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.datastructures.Transformation.OutputType
import exastencils.waLBerla.ir.blockforest._

object IR_WaLBerlaAABB {
  def datatype : IR_Datatype = IR_SpecialDatatype("math::AABB")
}

trait IR_WaLBerlaAABB extends IR_WaLBerlaBlockLoopVariable {

  def center(dim : IR_Expression) : IR_Expression = IR_ArrayAccess(IR_MemberFunctionCallWithDt(this, "center", IR_ArrayDatatype(IR_RealDatatype, 3)), dim)

  def min(dim : IR_Expression) : IR_Expression = IR_MemberFunctionCallWithDt(this, "min", IR_RealDatatype, dim)

  def max(dim : IR_Expression) : IR_Expression = IR_MemberFunctionCallWithDt(this, "max", IR_RealDatatype, dim)

  def size(dim : IR_Expression) : IR_Expression = IR_MemberFunctionCallWithDt(this, "size", IR_RealDatatype, dim)
}

case class IR_WaLBerlaBlockAABB(block : IR_WaLBerlaBlock) extends IR_WaLBerlaAABB {
  override def resolveName() : String = "blockAABB"
  override def resolveDatatype() : IR_Datatype = IR_WaLBerlaAABB.datatype

  override def getDeclaration() = IR_VariableDeclaration(resolveDatatype(), resolveName(),
    IR_MemberFunctionCallArrowWithDt(block, "getAABB", datatype))
}

case class IR_WaLBerlaCellAABB(blockforest : IR_WaLBerlaBlockForest, idx : IR_Index) extends IR_WaLBerlaAABB {
  override def resolveName() : String = "cellAABB"
  override def resolveDatatype() : IR_Datatype = IR_WaLBerlaAABB.datatype

  override def getDeclaration() = {
    IR_VariableDeclaration(resolveDatatype(), resolveName(),
      IR_MemberFunctionCallArrowWithDt(blockforest, "getBlockLocalCellAABB", datatype,
        IR_DerefAccess(IR_WaLBerlaLoopOverLocalBlocks.block), IR_FunctionCall(IR_ExternalFunctionReference("Cell"), idx.toExpressionIndex.indices.padTo(3, 0 : IR_Expression) : _*)))
  }
}
