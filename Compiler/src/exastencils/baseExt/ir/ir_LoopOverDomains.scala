package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge.DomainCollection
import exastencils.prettyprinting.PpStream

object IR_LoopOverDomains {
  def apply(body : IR_Statement*) = new IR_LoopOverDomains(body.to[ListBuffer])

  def defIt = "domainIdx"
}

case class IR_LoopOverDomains(var body : ListBuffer[IR_Statement]) extends IR_Statement with IR_Expandable {

  import IR_LoopOverDomains._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverDomains\n"

  override def expand() : Output[IR_ForLoop] = {
    IR_ForLoop(
      VariableDeclarationStatement(IR_IntegerDatatype, defIt, Some(0)),
      IR_LowerExpression(defIt, DomainCollection.domains.size),
      IR_PreIncrementExpression(defIt),
      body)
  }
}

