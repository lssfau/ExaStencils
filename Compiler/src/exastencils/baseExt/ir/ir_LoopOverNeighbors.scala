package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge.Fragment
import exastencils.prettyprinting.PpStream

object IR_LoopOverNeighbors {
  def apply(body : IR_Statement*) = new IR_LoopOverNeighbors(body.to[ListBuffer])

  def defIt = "neighborIdx"
}

case class IR_LoopOverNeighbors(var body : ListBuffer[IR_Statement]) extends IR_Statement with IR_Expandable {

  import IR_LoopOverNeighbors._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverNeighbors\n"

  override def expand() : Output[IR_ForLoop] = {
    IR_ForLoop(
      VariableDeclarationStatement(IR_IntegerDatatype, defIt, Some(0)),
      IR_LowerExpression(defIt, Fragment.neighbors.size),
      IR_PreIncrementExpression(defIt),
      body)
  }
}
