package exastencils.util.ir

import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures._

/// IR_ReplaceVariableAccess

object IR_ReplaceVariableAccess extends QuietDefaultStrategy("Replace something with something else") {
  var toReplace : String = ""
  var replacement : Node = IR_VariableAccess("") // to be overwritten

  this += new Transformation("Search and replace", {
// TODO: rely only on IR_VariableAccess => eliminate IR_StringLiteral occurrences
    case IR_StringLiteral(s) if s == toReplace                  => Duplicate(replacement)
    case access : IR_VariableAccess if access.name == toReplace => Duplicate(replacement)
  }, false)
}
