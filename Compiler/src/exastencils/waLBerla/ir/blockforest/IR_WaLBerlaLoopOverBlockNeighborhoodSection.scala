package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.datastructures.Transformation.OutputType
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_UintType
import exastencils.waLBerla.ir.util.IR_WaLBerlaDirection

object IR_WaLBerlaLoopOverBlockNeighborhoodSection {
  def apply(wbNeighDir : IR_Expression, body : IR_Statement*) = new IR_WaLBerlaLoopOverBlockNeighborhoodSection(wbNeighDir, body.to[ListBuffer])

  def apply(neighDir : Array[Int], body : IR_Statement*) = new IR_WaLBerlaLoopOverBlockNeighborhoodSection(IR_WaLBerlaDirection.getDirIndexFromArray(neighDir), body.to[ListBuffer])

  def wbNeighborHoodSectionIdx : IR_VariableAccess = IR_VariableAccess("neighborHoodSectionIdx", IR_SpecialDatatype("const auto"))
  def wbNeighborHoodSectionSize : IR_VariableAccess = IR_VariableAccess("neighborHoodSectionSize", IR_SpecialDatatype("const auto"))

  def wbNeighborIdx : IR_VariableAccess = IR_VariableAccess("neighIdx", WB_UintType)

  def defIt = "wbNeighborhoodIdx"
}

case class IR_WaLBerlaLoopOverBlockNeighborhoodSection(var wbNeighDir : IR_Expression, var body : ListBuffer[IR_Statement]) extends IR_ScopedStatement with IR_Expandable {
  override def expand() : OutputType = {
    import IR_WaLBerlaLoopOverBlockNeighborhoodSection._

    def block = IR_WaLBerlaLoopOverLocalBlocks.block

    var result = ListBuffer[IR_Statement]()

    // fetch neighborhood section and its size
    result += IR_VariableDeclaration(wbNeighborHoodSectionIdx,
      IR_FunctionCall("blockforest::getBlockNeighborhoodSectionIndex", IR_Cast(IR_SpecialDatatype("const stencil::Direction"), wbNeighDir)))
    result += IR_VariableDeclaration(wbNeighborHoodSectionSize,
      IR_MemberFunctionCallArrow(block, "getNeighborhoodSectionSize", wbNeighborHoodSectionIdx))

    result += IR_ForLoop(
      IR_VariableDeclaration(wbNeighborIdx, 0),
      IR_Lower(wbNeighborIdx, wbNeighborHoodSectionSize),
      IR_PreIncrement(wbNeighborIdx),
      body)

    result
  }
}
