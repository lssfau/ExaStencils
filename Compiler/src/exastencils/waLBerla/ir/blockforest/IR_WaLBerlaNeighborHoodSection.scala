package exastencils.waLBerla.ir.blockforest
import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.waLBerla.ir.util.IR_WaLBerlaDirection

/// IR_WaLBerlaNeighborHoodSectionIndex

case class IR_WaLBerlaNeighborHoodSectionIndex(var wbNeighDir : Array[Int]) extends IR_WaLBerlaBlockLoopVariable {

  def dirIndex = IR_WaLBerlaDirection.getDirIndexFromArray(wbNeighDir)

  override def resolveName() : String = "neighborHoodSectionIdx_" + dirIndex
  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("const auto")

  override def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(resolveDatatype(), resolveName(),
    IR_FunctionCall("blockforest::getBlockNeighborhoodSectionIndex", IR_Cast(IR_SpecialDatatype("const stencil::Direction"), dirIndex)))
}

/// IR_WaLBerlaNeighborHoodSectionSize

case class IR_WaLBerlaNeighborHoodSectionSize(var wbNeighborHoodSectionIdx : IR_WaLBerlaNeighborHoodSectionIndex) extends IR_WaLBerlaBlockLoopVariable {
  override def resolveName() : String = "neighborHoodSectionSize_" + wbNeighborHoodSectionIdx.dirIndex
  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("const auto")

  override def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(resolveDatatype(), resolveName(),
    IR_MemberFunctionCallArrow(IR_WaLBerlaLoopOverLocalBlocks.block, "getNeighborhoodSectionSize", wbNeighborHoodSectionIdx))
}