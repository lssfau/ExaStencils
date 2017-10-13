package exastencils.util.ir

import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger

/// IR_ReplaceExpressions

object IR_ReplaceExpressions extends QuietDefaultStrategy("Replace something with something else") {
  var toReplace : IR_Expression = IR_NullExpression
  var replacement : IR_Expression = IR_NullExpression

  this += new Transformation("Search and replace", {
    case e : IR_Expression if e == toReplace => Duplicate(replacement)
    //case e : IR_Expression if e.toString == toReplace.toString           => Duplicate(replacement)
    case e : IR_Expression if e.prettyprint() == toReplace.prettyprint() =>
      Logger.warn(s"$e doesn't seem to be $toReplace")
      e
  }, false)
}

/// IR_ReplaceIntWithReal

object IR_ReplaceIntWithReal extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case IR_IntegerConstant(c) => IR_RealConstant(c)
  })
}

/// IR_ReplaceRealWithInt

object IR_ReplaceRealWithInt extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case IR_RealConstant(c) => IR_IntegerConstant(c.toInt)
  })
}
