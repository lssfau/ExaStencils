package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.domain.ir.IR_DomainCollection

object IR_LoopOverDomains {
  def apply(body : IR_Statement*) = new IR_LoopOverDomains(body.to[ListBuffer])

  def defIt = "domainIdx"
}

case class IR_LoopOverDomains(var body : ListBuffer[IR_Statement]) extends IR_ScopedStatement with IR_Expandable {

  import IR_LoopOverDomains._

  override def expand() : Output[IR_ForLoop] = {
    IR_ForLoop(
      IR_VariableDeclaration(IR_IntegerDatatype, defIt, 0),
      IR_Lower(defIt, IR_DomainCollection.objects.size),
      IR_PreIncrement(defIt),
      body)
  }
}

