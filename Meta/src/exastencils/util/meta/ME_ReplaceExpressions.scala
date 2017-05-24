package exastencils.util.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_ReplaceExpressions extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/util/|LAYER_LC|/|LAYER_UC|_ReplaceExpressions.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.util.|LAYER_LC|

import exastencils.base.|LAYER_LC|._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger

/// |LAYER_UC|_ReplaceExpressions

object |LAYER_UC|_ReplaceExpressions extends QuietDefaultStrategy("Replace something with something else") {
  var toReplace : |LAYER_UC|_Expression = |LAYER_UC|_NullExpression
  var replacement : |LAYER_UC|_Expression = |LAYER_UC|_NullExpression

  this += new Transformation("Search and replace", {
    case e : |LAYER_UC|_Expression if e == toReplace => Duplicate(replacement)
    //case e : |LAYER_UC|_Expression if e.toString == toReplace.toString           => Duplicate(replacement)
    case e : |LAYER_UC|_Expression if e.prettyprint() == toReplace.prettyprint() =>
      Logger.warn(s"$e doesn't seem to be $toReplace")
      e
  }, false)
}

/// |LAYER_UC|_ReplaceIntWithReal

object |LAYER_UC|_ReplaceIntWithReal extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case |LAYER_UC|_IntegerConstant(c) => |LAYER_UC|_RealConstant(c)
  })
}

/// |LAYER_UC|_ReplaceRealWithInt

object |LAYER_UC|_ReplaceRealWithInt extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case |LAYER_UC|_RealConstant(c) => |LAYER_UC|_IntegerConstant(c.toInt)
  })
}
"""
  }
}
