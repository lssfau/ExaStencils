package exastencils.base.l3

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_IfCondition
import exastencils.prettyprinting._

/// L3_IfCondition

object L3_IfCondition {
  def apply(condition : L3_Expression, trueBody : L3_Statement) = new L3_IfCondition(condition, ListBuffer(trueBody), ListBuffer())
  def apply(condition : L3_Expression, trueBody : ListBuffer[L3_Statement]) = new L3_IfCondition(condition, trueBody, ListBuffer())
}

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

  override def progress = ProgressLocation(L4_IfCondition(condition.progress, trueBody.map(_.progress), falseBody.map(_.progress)))
}
