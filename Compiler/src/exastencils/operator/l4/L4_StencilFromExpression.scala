package exastencils.operator.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.optimization.l4.L4_GeneralSimplify
import exastencils.prettyprinting._

/// L4_StencilFromExpression

case class L4_StencilFromExpression(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var expression : L4_Expression) extends L4_StencilDecl {

  override def prettyprint(out : PpStream) = {
    out << "Stencil " << name
    if (levels.isDefined) out << "@" << levels.get
    out << " from " << expression
  }

  override def addToKnowledge() : Unit = {
    while (!expression.isInstanceOf[L4_StencilAccess]) {
      L4_ResolveStencilExpression.applyStandalone(this)
      if (0 == L4_ResolveStencilExpression.results.last._2.matches)
        Logger.error(s"Not able to resolve expression in stencil declaration for $name; expression: $expression")
      L4_GeneralSimplify.doUntilDoneStandalone(this)
    }

    val stencil = expression.asInstanceOf[L4_StencilAccess].target
    stencil.name = name
    stencil.level = levels.get.resolveLevel

    L4_StencilCollection.add(stencil)
  }
}

/// L4_ResolveStencilExpression

object L4_ResolveStencilExpression extends DefaultStrategy("Resolve operations involving stencils") {
  this += new Transformation("Resolve", {
    case add : L4_Addition if add.summands.exists(_.isInstanceOf[L4_StencilAccess]) =>
      val (stencilAccesses, other) : (ListBuffer[L4_Expression], ListBuffer[L4_Expression]) = add.summands.partition(_.isInstanceOf[L4_StencilAccess])
      val stencils = stencilAccesses.map(_.asInstanceOf[L4_StencilAccess].target)
      var newStencil = stencils.remove(stencils.length - 1)
      while (stencils.nonEmpty)
        newStencil = L4_StencilOps.add(stencils.remove(stencils.length - 1), newStencil)

      if (other.nonEmpty)
        L4_Addition(L4_StencilAccess(newStencil) +: other)
      else
        L4_StencilAccess(newStencil)

    case mul : L4_Multiplication if mul.factors.exists(_.isInstanceOf[L4_StencilAccess]) =>
      val factors : ListBuffer[L4_Expression] = mul.factors
      var result = ListBuffer[L4_Expression]()
      var prev : L4_Expression = null

      prev = null
      for (f <- factors)
        (prev, f) match {
          case (left : L4_StencilAccess, right : L4_StencilAccess) =>
            prev = L4_StencilAccess(L4_StencilOps.mul(left.target, right.target))
          case (left : L4_StencilAccess, right : L4_Number)        =>
            prev = L4_StencilAccess(L4_StencilOps.scale(left.target, right))
          case (left : L4_Number, right : L4_StencilAccess)        =>
            prev = L4_StencilAccess(L4_StencilOps.scale(right.target, left))
          case _                                                   =>
            if (prev != null) result += prev
            prev = f
        }
      if (prev != null)
        result += prev

      L4_Multiplication(result)

    case L4_FunctionCall(fctRef : L4_FunctionReference, ListBuffer(toTranspose : L4_StencilAccess)) if "transpose" == fctRef.name =>
      L4_StencilAccess(L4_StencilOps.transpose(toTranspose.target))

    case L4_FunctionCall(fctRef : L4_FunctionReference, args) if "kron" == fctRef.name =>
      def processArgs(arguments : ListBuffer[L4_Expression]) : L4_Expression = {
        if (1 == arguments.length) {
          arguments.head
        } else {
          val processed = (arguments(0), arguments(1)) match {
            case (left : L4_Number, right : L4_StencilAccess) =>
              Logger.warn("Found constant in kron, assuming stencil scaling is implied")
              L4_StencilAccess(L4_StencilOps.scale(right.target, left))

            case (left : L4_Number, right : L4_Number) =>
              Logger.warn("Found constant in kron, assuming stencil scaling is implied")
              left * right

            case (left : L4_StencilAccess, right : L4_Number) =>
              Logger.warn("Found constant in kron, assuming stencil scaling is implied")
              L4_StencilAccess(L4_StencilOps.scale(left.target, right))

            case (left : L4_StencilAccess, right : L4_StencilAccess) =>
              L4_StencilAccess(L4_StencilOps.kron(left.target, right.target))

            case other =>
              Logger.error(s"Unsupported arguments in kron: ${ other._1 } or ${ other._2 }")
          }
          processArgs(processed +: arguments.drop(2))
        }
      }

      processArgs(args)
  })
}
