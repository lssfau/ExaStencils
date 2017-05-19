package exastencils.util.l2

import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger

/// L2_ReplaceExpressions

object L2_ReplaceExpressions extends QuietDefaultStrategy("Replace something with something else") {
  var toReplace : L2_Expression = L2_NullExpression
  var replacement : L2_Expression = L2_NullExpression

  this += new Transformation("Search and replace", {
    case e : L2_Expression if e == toReplace => Duplicate(replacement)
    //case e : L2_Expression if e.toString == toReplace.toString           => Duplicate(replacement)
    case e : L2_Expression if e.prettyprint() == toReplace.prettyprint() =>
      Logger.warn(s"$e doesn't seem to be $toReplace")
      e
  }, false)
}

/// L2_ReplaceIntWithReal

object L2_ReplaceIntWithReal extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case L2_IntegerConstant(c) => L2_RealConstant(c)
  })
}

/// L2_ReplaceRealWithInt

object L2_ReplaceRealWithInt extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case L2_RealConstant(c) => L2_IntegerConstant(c.toInt)
  })
}
