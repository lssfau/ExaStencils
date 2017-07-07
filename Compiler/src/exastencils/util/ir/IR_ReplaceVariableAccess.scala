package exastencils.util.ir

import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures._

/// IR_ReplaceVariableAccess

object IR_ReplaceVariableAccess extends QuietDefaultStrategy("Replace something with something else") {
  var replace : Map[String, Node] = null // must be replaced

  this += new Transformation("Search and replace", {
// TODO: rely only on IR_VariableAccess => eliminate IR_StringLiteral occurrences
    case IR_VariableAccess(str, _) if replace.isDefinedAt(str) => Duplicate(replace(str))
    case IR_StringLiteral(str) if replace.isDefinedAt(str)     => Duplicate(replace(str))
  }, false)
}
