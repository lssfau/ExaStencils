package exastencils.util.l4

import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger

/// L4_ReplaceExpressions

object L4_ReplaceExpressions extends QuietDefaultStrategy("Replace something with something else") {
  var toReplace : L4_Expression = L4_NullExpression
  var replacement : L4_Expression = L4_NullExpression

  this += new Transformation("Search and replace", {
    case e : L4_Expression if e == toReplace => Duplicate(replacement)
    //case e : L4_Expression if e.toString == toReplace.toString           => Duplicate(replacement)
    case e : L4_Expression if e.prettyprint() == toReplace.prettyprint() =>
      Logger.warn(s"$e doesn't seem to be $toReplace")
      e
  }, false)
}

/// L4_ReplaceIntWithReal

object L4_ReplaceIntWithReal extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case L4_IntegerConstant(c) => L4_RealConstant(c)
  })
}

/// L4_ReplaceRealWithInt

object L4_ReplaceRealWithInt extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case L4_RealConstant(c) => L4_IntegerConstant(c.toInt)
  })
}
