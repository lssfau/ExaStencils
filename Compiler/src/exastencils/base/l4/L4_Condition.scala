package exastencils.base.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_IfCondition
import exastencils.prettyprinting.PpStream

/// L4_IfCondition

case class L4_IfCondition(var condition : L4_Expression, var trueBody : ListBuffer[L4_Statement], var falseBody : ListBuffer[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "if ( " << condition << " ) {\n"
    out <<< (trueBody, "\n")
    if (falseBody.nonEmpty) {
      out << "\n} else {\n"
      out <<< (falseBody, "\n")
    }
    out << "\n}"
  }

  override def progress = ProgressLocation(IR_IfCondition(condition.progress, trueBody.map(_.progress), falseBody.map(_.progress)))
}
