package exastencils.datastructures.l4

import exastencils.base.l4._
import exastencils.logger._
import exastencils.prettyprinting._

trait HasIdentifier {
  var identifier : Identifier
}

case class LeveledScopeStatement(var level : LevelSpecification, var statements : List[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << level << " {\n"
    statements.foreach(_.prettyprint(out))
    out << "\n}\n"
  }
  override def progress = {
    Logger.error("cannot progress LeveledScopeStatement to IR")
  }
}
