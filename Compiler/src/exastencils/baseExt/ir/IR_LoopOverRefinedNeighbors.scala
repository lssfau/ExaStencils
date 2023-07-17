package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output

object IR_LoopOverRefinedNeighbors {
  def apply(body : IR_Statement*) = new IR_LoopOverRefinedNeighbors(body.to[ListBuffer])

  def defIt = "refinedNeighborIdx"
}

case class IR_LoopOverRefinedNeighbors(var body : ListBuffer[IR_Statement]) extends IR_ScopedStatement with IR_Expandable {

  import IR_LoopOverRefinedNeighbors._

  override def expand() : Output[IR_ForLoop] = {
    IR_ForLoop(
      IR_VariableDeclaration(IR_IntegerDatatype, defIt, 0),
      IR_Lower(defIt, Knowledge.refinement_maxCommNeighborsPerDir),
      IR_PreIncrement(defIt),
      body)
  }
}
