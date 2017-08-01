package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.optimization.l3.L3_GeneralSimplify
import exastencils.prettyprinting._

/// L3_StencilFromExpression

case class L3_StencilFromExpression(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var expression : L3_Expression) extends L3_StencilDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"

  override def addToKnowledge() : Unit = {
    while (!expression.isInstanceOf[L3_StencilAccess]) {
      L3_ResolveStencilExpression.applyStandalone(this)
      if (0 == L3_ResolveStencilExpression.results.last._2.matches)
        Logger.error(s"Not able to resolve expression in stencil declaration for $name; expression: $expression")
      L3_GeneralSimplify.doUntilDoneStandalone(this)
    }

    val stencil = expression.asInstanceOf[L3_StencilAccess].target
    stencil.name = name
    stencil.level = levels.get.resolveLevel

    L3_StencilCollection.add(stencil)
  }
}

/// L3_ResolveStencilExpression

object L3_ResolveStencilExpression extends DefaultStrategy("Resolve operations involving stencils") {
  this += new Transformation("Resolve", {
    case add : L3_Addition if add.summands.exists(_.isInstanceOf[L3_StencilAccess]) =>
      val (stencilAccesses, other) : (ListBuffer[L3_Expression], ListBuffer[L3_Expression]) = add.summands.partition(_.isInstanceOf[L3_StencilAccess])
      val stencils = stencilAccesses.map(_.asInstanceOf[L3_StencilAccess].target)
      var newStencil = stencils.remove(stencils.length - 1)
      while (stencils.nonEmpty)
        newStencil = L3_StencilOps.add(stencils.remove(stencils.length - 1), newStencil)

      if (other.nonEmpty)
        L3_Addition(L3_StencilAccess(newStencil) +: other)
      else
        L3_StencilAccess(newStencil)

    case mul : L3_Multiplication if mul.factors.exists(_.isInstanceOf[L3_StencilAccess]) =>
      val factors : ListBuffer[L3_Expression] = mul.factors
      var result = ListBuffer[L3_Expression]()
      var prev : L3_Expression = null

      prev = null
      for (f <- factors)
        (prev, f) match {
          case (left : L3_StencilAccess, right : L3_StencilAccess) =>
            prev = L3_StencilAccess(L3_StencilOps.mul(left.target, right.target))
          case (left : L3_StencilAccess, right : L3_Number)        =>
            prev = L3_StencilAccess(L3_StencilOps.scale(left.target, right))
          case (left : L3_Number, right : L3_StencilAccess)        =>
            prev = L3_StencilAccess(L3_StencilOps.scale(right.target, left))
          case _                                                   =>
            if (prev != null) result += prev
            prev = f
        }
      if (prev != null)
        result += prev

      L3_Multiplication(result)

    case L3_FunctionCall(fctRef : L3_FunctionReference, ListBuffer(toTranspose : L3_StencilAccess)) if "transpose" == fctRef.name =>
      L3_StencilAccess(L3_StencilOps.transpose(toTranspose.target))

    case L3_FunctionCall(fctRef : L3_FunctionReference, args) if "kron" == fctRef.name =>
      def processArgs(arguments : ListBuffer[L3_Expression]) : L3_Expression = {
        if (1 == arguments.length) {
          arguments.head
        } else {
          val processed = (arguments(0), arguments(1)) match {
            case (left : L3_Number, right : L3_StencilAccess) =>
              Logger.warn("Found constant in kron, assuming stencil scaling is implied")
              L3_StencilAccess(L3_StencilOps.scale(right.target, left))

            case (left : L3_Number, right : L3_Number) =>
              Logger.warn("Found constant in kron, assuming stencil scaling is implied")
              left * right

            case (left : L3_StencilAccess, right : L3_Number) =>
              Logger.warn("Found constant in kron, assuming stencil scaling is implied")
              L3_StencilAccess(L3_StencilOps.scale(left.target, right))

            case (left : L3_StencilAccess, right : L3_StencilAccess) =>
              L3_StencilAccess(L3_StencilOps.kron(left.target, right.target))

            case other =>
              Logger.error(s"Unsupported arguments in kron: ${ other._1 } or ${ other._2 }")
          }
          processArgs(processed +: arguments.drop(2))
        }
      }

      processArgs(args)
  })
}
