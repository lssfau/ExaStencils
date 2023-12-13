package exastencils.waLBerla.ir.grid

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.waLBerla.ir.blockforest._

object IR_WaLBerlaAABB {
  def datatype : IR_Datatype = IR_SpecialDatatype("math::AABB")
}

trait IR_WaLBerlaAABB extends IR_WaLBerlaBlockLoopVariable

case class IR_WaLBerlaAABBCenter(var aabb : IR_WaLBerlaAABB, dim : Int) extends IR_WaLBerlaBlockLoopVariable {
  override def resolveName() : String = "aabbCenter_" + ('x' + dim).toChar.toString
  override def resolveDatatype() : IR_Datatype = IR_RealDatatype

  override def getDeclaration() = IR_VariableDeclaration(resolveDatatype(), resolveName(),
    IR_ArrayAccess(IR_MemberFunctionCallWithDt(aabb, "center", IR_ArrayDatatype(IR_RealDatatype, 3)), dim))
}

case class IR_WaLBerlaAABBMin(var aabb : IR_WaLBerlaAABB, dim : Int) extends IR_WaLBerlaBlockLoopVariable {
  override def resolveName() : String = "aabbMin_" + ('x' + dim).toChar.toString
  override def resolveDatatype() : IR_Datatype = IR_RealDatatype

  override def getDeclaration() = IR_VariableDeclaration(resolveDatatype(), resolveName(),
    IR_MemberFunctionCallWithDt(aabb, "min", IR_RealDatatype, dim))
}

case class IR_WaLBerlaAABBMax(var aabb : IR_WaLBerlaAABB, dim : Int) extends IR_WaLBerlaBlockLoopVariable {
  override def resolveName() : String = "aabbMax_" + ('x' + dim).toChar.toString
  override def resolveDatatype() : IR_Datatype = IR_RealDatatype

  override def getDeclaration() = IR_VariableDeclaration(resolveDatatype(), resolveName(),
    IR_MemberFunctionCallWithDt(aabb, "max", IR_RealDatatype, dim))
}

case class IR_WaLBerlaAABBSize(var aabb : IR_WaLBerlaAABB, dim : Int) extends IR_WaLBerlaBlockLoopVariable {
  override def resolveName() : String = "aabbSize_" + ('x' + dim).toChar.toString
  override def resolveDatatype() : IR_Datatype = IR_RealDatatype

  override def getDeclaration() = IR_VariableDeclaration(resolveDatatype(), resolveName(),
    IR_MemberFunctionCallWithDt(aabb, "size", IR_RealDatatype, dim))
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
