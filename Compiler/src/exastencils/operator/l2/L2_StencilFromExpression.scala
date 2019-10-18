//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.operator.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.optimization.l2.L2_GeneralSimplify
import exastencils.prettyprinting._

/// L2_StencilFromExpression

case class L2_StencilFromExpression(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var expression : L2_Expression) extends L2_StencilDecl {

  override def prettyprint(out : PpStream) = {
    out << "Operator " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " from " << expression
  }

  override def addToKnowledge() : Unit = {
    while (!expression.isInstanceOf[L2_StencilAccess]) {
      L2_ResolveStencilExpression.applyStandalone(this)
      if (0 == L2_ResolveStencilExpression.results.last._2.matches)
        Logger.error(s"Not able to resolve expression in stencil declaration for $name; expression: $expression")
      L2_GeneralSimplify.doUntilDoneStandalone(this)
    }

    val stencil = expression.asInstanceOf[L2_StencilAccess].target
    stencil.name = name
    stencil.level = levels.get.resolveLevel

    L2_StencilCollection.add(stencil)
  }
}

/// L2_ResolveStencilExpression

object L2_ResolveStencilExpression extends DefaultStrategy("Resolve operations involving stencils") {
  this += new Transformation("Resolve", {
    case add : L2_Addition if add.summands.exists(_.isInstanceOf[L2_StencilAccess]) =>
      val (stencilAccesses, other) : (ListBuffer[L2_Expression], ListBuffer[L2_Expression]) = add.summands.partition(_.isInstanceOf[L2_StencilAccess])
      val stencils = stencilAccesses.map(_.asInstanceOf[L2_StencilAccess].target)
      var newStencil = stencils.remove(stencils.length - 1)
      while (stencils.nonEmpty)
        newStencil = L2_StencilOps.add(stencils.remove(stencils.length - 1), newStencil)

      if (other.nonEmpty)
        L2_Addition(L2_StencilAccess(newStencil) +: other)
      else
        L2_StencilAccess(newStencil)

    case mul : L2_Multiplication if mul.factors.exists(_.isInstanceOf[L2_StencilAccess]) =>
      val factors : ListBuffer[L2_Expression] = mul.factors
      var result = ListBuffer[L2_Expression]()
      var prev : L2_Expression = null

      prev = null
      for (f <- factors)
        (prev, f) match {
          case (left : L2_StencilAccess, right : L2_StencilAccess) =>
            prev = L2_StencilAccess(L2_StencilOps.mul(left.target, right.target))
          case (left : L2_StencilAccess, right : L2_Number)        =>
            prev = L2_StencilAccess(L2_StencilOps.scale(left.target, right))
          case (left : L2_Number, right : L2_StencilAccess)        =>
            prev = L2_StencilAccess(L2_StencilOps.scale(right.target, left))
          case _                                                   =>
            if (prev != null) result += prev
            prev = f
        }
      if (prev != null)
        result += prev

      L2_Multiplication(result)

    case L2_FunctionCall(fctRef : L2_FunctionReference, ListBuffer(toTranspose : L2_StencilAccess)) if "transpose" == fctRef.name =>
      L2_StencilAccess(L2_StencilOps.transpose(toTranspose.target))

    case L2_FunctionCall(fctRef : L2_FunctionReference, args) if "kron" == fctRef.name =>
      def processArgs(arguments : ListBuffer[L2_Expression]) : L2_Expression = {
        if (1 == arguments.length) {
          arguments.head
        } else {
          val processed = (arguments(0), arguments(1)) match {
            case (left : L2_Number, right : L2_StencilAccess) =>
              Logger.warn("Found constant in kron, assuming stencil scaling is implied")
              L2_StencilAccess(L2_StencilOps.scale(right.target, left))

            case (left : L2_Number, right : L2_Number) =>
              Logger.warn("Found constant in kron, assuming stencil scaling is implied")
              left * right

            case (left : L2_StencilAccess, right : L2_Number) =>
              Logger.warn("Found constant in kron, assuming stencil scaling is implied")
              L2_StencilAccess(L2_StencilOps.scale(left.target, right))

            case (left : L2_StencilAccess, right : L2_StencilAccess) =>
              L2_StencilAccess(L2_StencilOps.kron(left.target, right.target))

            case other =>
              Logger.error(s"Unsupported arguments in kron: ${ other._1 } or ${ other._2 }")
          }
          processArgs(processed +: arguments.drop(2))
        }
      }

      processArgs(args)
  })
}
