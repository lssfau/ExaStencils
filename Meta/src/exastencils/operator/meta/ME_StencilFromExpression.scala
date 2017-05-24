package exastencils.operator.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_StencilFromExpression extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/operator/|LAYER_LC|/|LAYER_UC|_StencilFromExpression.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.operator.|LAYER_LC|

import scala.collection.mutable.ListBuffer

import exastencils.base.|LAYER_LC|._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.optimization.|LAYER_LC|.|LAYER_UC|_GeneralSimplify
import exastencils.prettyprinting._

/// |LAYER_UC|_StencilFromExpression

case class |LAYER_UC|_StencilFromExpression(
    var name : String,
    var levels : Option[|LAYER_UC|_LevelSpecification],
    var expression : |LAYER_UC|_Expression) extends |LAYER_UC|_StencilDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"

  override def addToKnowledge() : Unit = {
    while (!expression.isInstanceOf[|LAYER_UC|_StencilAccess]) {
      |LAYER_UC|_ResolveStencilExpression.applyStandalone(this)
      if (0 == |LAYER_UC|_ResolveStencilExpression.results.last._2.matches)
        Logger.error(s"Not able to resolve expression in stencil declaration for $name; expression: $expression")
      |LAYER_UC|_GeneralSimplify.applyStandalone(this)
    }

    val stencil = expression.asInstanceOf[|LAYER_UC|_StencilAccess].target
    stencil.name = name
    stencil.level = levels.get.asInstanceOf[|LAYER_UC|_SingleLevel].level

    |LAYER_UC|_StencilCollection.add(stencil)
  }
}

/// |LAYER_UC|_ResolveStencilExpression

object |LAYER_UC|_ResolveStencilExpression extends DefaultStrategy("Resolve operations involving stencils") {
  this += new Transformation("Resolve", {
    case add : |LAYER_UC|_Addition if add.summands.exists(_.isInstanceOf[|LAYER_UC|_StencilAccess]) =>
      val (stencilAccesses, other) : (ListBuffer[|LAYER_UC|_Expression], ListBuffer[|LAYER_UC|_Expression]) = add.summands.partition(_.isInstanceOf[|LAYER_UC|_StencilAccess])
      val stencils = stencilAccesses.map(_.asInstanceOf[|LAYER_UC|_StencilAccess].target)
      var newStencil = stencils.remove(stencils.length - 1)
      while (stencils.nonEmpty)
        newStencil = |LAYER_UC|_StencilOps.add(stencils.remove(stencils.length - 1), newStencil)

      if (other.nonEmpty)
        |LAYER_UC|_Addition(|LAYER_UC|_StencilAccess(newStencil) +: other)
      else
        |LAYER_UC|_StencilAccess(newStencil)

    case mul : |LAYER_UC|_Multiplication if mul.factors.exists(_.isInstanceOf[|LAYER_UC|_StencilAccess]) =>
      val factors : ListBuffer[|LAYER_UC|_Expression] = mul.factors
      var result = ListBuffer[|LAYER_UC|_Expression]()
      var prev : |LAYER_UC|_Expression = null

      prev = null
      for (f <- factors)
        (prev, f) match {
          case (left : |LAYER_UC|_StencilAccess, right : |LAYER_UC|_StencilAccess) =>
            prev = |LAYER_UC|_StencilAccess(|LAYER_UC|_StencilOps.mul(left.target, right.target))
          case (left : |LAYER_UC|_StencilAccess, right : |LAYER_UC|_Number)        =>
            prev = |LAYER_UC|_StencilAccess(|LAYER_UC|_StencilOps.scale(left.target, right))
          case (left : |LAYER_UC|_Number, right : |LAYER_UC|_StencilAccess)        =>
            prev = |LAYER_UC|_StencilAccess(|LAYER_UC|_StencilOps.scale(right.target, left))
          case _                                                   =>
            if (prev != null) result += prev
            prev = f
        }
      if (prev != null)
        result += prev

      |LAYER_UC|_Multiplication(result)

    case |LAYER_UC|_FunctionCall(fctAccess : |LAYER_UC|_Access, ListBuffer(toTranspose : |LAYER_UC|_StencilAccess)) if "transpose" == fctAccess.name =>
      |LAYER_UC|_StencilAccess(|LAYER_UC|_StencilOps.transpose(toTranspose.target))

    case |LAYER_UC|_FunctionCall(fctAccess : |LAYER_UC|_Access, args) if "kron" == fctAccess.name =>
      def processArgs(arguments : ListBuffer[|LAYER_UC|_Expression]) : |LAYER_UC|_Expression = {
        if (1 == arguments.length) {
          arguments.head
        } else {
          val processed = (arguments(0), arguments(1)) match {
            case (left : |LAYER_UC|_Number, right : |LAYER_UC|_StencilAccess) =>
              Logger.warn("Found constant in kron, assuming stencil scaling is implied")
              |LAYER_UC|_StencilAccess(|LAYER_UC|_StencilOps.scale(right.target, left))

            case (left : |LAYER_UC|_Number, right : |LAYER_UC|_Number) =>
              Logger.warn("Found constant in kron, assuming stencil scaling is implied")
              left * right

            case (left : |LAYER_UC|_StencilAccess, right : |LAYER_UC|_Number) =>
              Logger.warn("Found constant in kron, assuming stencil scaling is implied")
              |LAYER_UC|_StencilAccess(|LAYER_UC|_StencilOps.scale(left.target, right))

            case (left : |LAYER_UC|_StencilAccess, right : |LAYER_UC|_StencilAccess) =>
              |LAYER_UC|_StencilAccess(|LAYER_UC|_StencilOps.kron(left.target, right.target))

            case other =>
              Logger.error(s"Unsupported arguments in kron: ${ other._1 } or ${ other._2 }")
          }
          processArgs(processed +: arguments.drop(2))
        }
      }

      processArgs(args)
  })
}
"""
  }
}
