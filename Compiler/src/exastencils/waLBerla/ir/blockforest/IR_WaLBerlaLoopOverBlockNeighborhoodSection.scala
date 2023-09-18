package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.Output
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_UintType

/// IR_WaLBerlaLoopOverBlockNeighborhoodSection

object IR_WaLBerlaLoopOverBlockNeighborhoodSection {

  def apply(neighDir : Array[Int], body : IR_Statement*) = new IR_WaLBerlaLoopOverBlockNeighborhoodSection(neighDir, body.to[ListBuffer])

  def defIt = IR_VariableAccess("wbNeighborhoodIdx", WB_UintType)
}

case class IR_WaLBerlaLoopOverBlockNeighborhoodSection(var wbNeighDir : Array[Int], var body : ListBuffer[IR_Statement]) extends IR_ScopedStatement with IR_SpecialExpandable {
  private val wbNeighborHoodSectionIdx = IR_WaLBerlaNeighborHoodSectionIndex(wbNeighDir)
  private val wbNeighborHoodSectionSize = IR_WaLBerlaNeighborHoodSectionSize(wbNeighborHoodSectionIdx)

  def expandSpecial() : Output[IR_Statement] = {
    import IR_WaLBerlaLoopOverBlockNeighborhoodSection._

    var result = ListBuffer[IR_Statement]()

    result += IR_ForLoop(
      IR_VariableDeclaration(defIt, 0),
      IR_Lower(defIt, wbNeighborHoodSectionSize),
      IR_PreIncrement(defIt),
      body)

    IR_Scope(result)
  }
}

/// IR_ResolveWaLBerlaLoopOverBlockNeighborhoodSection

object IR_ResolveWaLBerlaLoopOverBlockNeighborhoodSection extends DefaultStrategy("Resolve loops over wb neighborhood sections") {
  this += Transformation("..", {
    case loop : IR_WaLBerlaLoopOverBlockNeighborhoodSection =>
      loop.expandSpecial()
  })
}
