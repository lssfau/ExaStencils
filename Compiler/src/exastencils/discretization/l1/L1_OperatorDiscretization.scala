package exastencils.discretization.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.l1.L1_ImplicitConversion._
import exastencils.base.l1._
import exastencils.config.Knowledge
import exastencils.domain.l1._
import exastencils.logger.Logger
import exastencils.operator.l1._
import exastencils.optimization.l1.L1_GeneralSimplify
import exastencils.prettyprinting.PpStream

/// L1_OperatorDiscretization

object L1_OperatorDiscretization {
  def apply(src : String, levels : Option[L1_DeclarationLevelSpecification], mapping : Option[String], discretization : Option[String], domain : String, order : Option[Int], direction : Option[Int]) =
    new L1_OperatorDiscretization(src, levels, mapping, discretization, L1_FutureDomainAccess(domain), order, direction)
}

case class L1_OperatorDiscretization(
    var src : String,
    var levels : Option[L1_DeclarationLevelSpecification],
    var mapping : Option[String],
    var discretization : Option[String],
    var domain : L1_Access,
    var order : Option[Int],
    var direction : Option[Int]) extends L1_DiscretizationHint {

  override def prettyprint(out : PpStream) = {
    out << src
    if (mapping.isDefined) out << " => " << mapping.get
    if (discretization.isDefined) out << " with " << L1_StringConstant(discretization.get)
    out << " on " << domain
  }

  def squashStencilExpr(expr : L1_Expression) : L1_Stencil = {
    expr match {
      case L1_StencilAccess(stencil) => stencil

      case L1_Negative(e) => L1_StencilOps.scale(squashStencilExpr(e), -1)

      case L1_Addition(summands)       => summands.map(squashStencilExpr).reduce(L1_StencilOps.add)
      case L1_Subtraction(left, right) => L1_StencilOps.add(squashStencilExpr(left), L1_StencilOps.scale(squashStencilExpr(right), -1))

      case L1_Multiplication(factors) => factors.map(squashStencilExpr).reduce(L1_StencilOps.mul)
      case L1_Division(left, right)   => L1_StencilOps.mul(squashStencilExpr(left), L1_StencilOps.inverse(squashStencilExpr(right)))

      case other => Logger.error(s"Unexpected: $other")
    }
  }

  def discretizeExpression(level : Int, expr : L1_Expression) : L1_Stencil = {
    discretization.getOrElse(Knowledge.discr_type).toLowerCase() match {
      case "fd" | "finitedifference" | "finitedifferences" | "finite_difference" | "finite_differences" | "finite difference" | "finite differences" =>
        L1_FD_DiscretizeSubtree.domain = domain.asInstanceOf[L1_DomainAccess].target
        L1_FD_DiscretizeSubtree.level = level
        L1_FD_DiscretizeSubtree.errOrder = order.getOrElse(Knowledge.discr_fd_order)
        L1_FD_DiscretizeSubtree.direction = direction.getOrElse(0)

        val wrapped = L1_ExpressionStatement(expr)
        L1_GeneralSimplify.applyStandalone(wrapped)
        L1_FD_DiscretizeSubtree.applyStandalone(wrapped)

        squashStencilExpr(wrapped.expression)

      case _ => Logger.error(s"Unsupported discretization scheme $discretization")
    }
  }

  override def process() = {
    val operators = {
      if (levels.isDefined) // explicit level definition is given -> extract levels and map to operators
        L1_LevelSpecification.extractLevelListDefEmpty(levels).to[ListBuffer].map(lvl => L1_OperatorCollection.getByIdentifier(src, lvl).get)
      else // otherwise collect all operators with the src name
        L1_OperatorCollection.getAllByIdentifier(src)
    }

    operators.foreach(l1Operator => {
      val tmpStencil = discretizeExpression(l1Operator.level, l1Operator.expr)
      tmpStencil.name = src

      val l2Operator = tmpStencil.progress()
      if (mapping.isDefined) l2Operator.name = mapping.get

      l1Operator.overwriteProgressed(l2Operator)
    })
  }
}
