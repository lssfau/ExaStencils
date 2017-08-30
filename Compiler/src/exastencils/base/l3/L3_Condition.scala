package exastencils.base.l3

import scala.collection.mutable._

import exastencils.base.l4.L4_IfCondition
import exastencils.prettyprinting._

/// L3_IfCondition

case class L3_IfCondition(var condition : L3_Expression, var trueBody : ListBuffer[L3_Statement], var falseBody : ListBuffer[L3_Statement]) extends L3_Statement {
  override def prettyprint(out : PpStream) = {
    out << "if ( " << condition << " ) {\n"
    out <<< (trueBody, "\n")
    if (falseBody.nonEmpty) {
      out << "\n} else {\n"
      out <<< (falseBody, "\n")
    }
    out << "\n}"
  }

  override def progress = L4_IfCondition(condition.progress, trueBody.map(_.progress), falseBody.map(_.progress))
}
