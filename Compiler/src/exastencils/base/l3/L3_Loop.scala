package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.base.l4._
import exastencils.prettyprinting._

/// L3_ForLoop

case class L3_ForLoop(
    var number : Int,
    var iterator : Option[L3_Access],
    var body : ListBuffer[L3_Statement]) extends L3_Statement {

  override def prettyprint(out : PpStream) = {
    out << "repeat " << number << " times"
    if (iterator.isDefined) out << " count " << iterator.get
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = ProgressLocation(L4_ForLoop(number, L3_ProgressOption(iterator)(_.progress), body.map(_.progress)))
}

/// L3_UntilLoop

case class L3_UntilLoop(var comparison : L3_Expression, var body : ListBuffer[L3_Statement]) extends L3_Statement {
  override def prettyprint(out : PpStream) = out << "repeat until " << comparison << " {\n" <<< (body, "\n") << "\n}"
  override def progress = ProgressLocation(L4_UntilLoop(comparison.progress, body.map(_.progress)))
}

/// L3_WhileLoop

case class L3_WhileLoop(var comparison : L3_Expression, var body : ListBuffer[L3_Statement]) extends L3_Statement {
  override def prettyprint(out : PpStream) = { out << "repeat while " << comparison << " {\n" <<< body << "}\n" }

  override def progress = ProgressLocation(L4_WhileLoop(comparison.progress, body.map(_.progress)))
}
