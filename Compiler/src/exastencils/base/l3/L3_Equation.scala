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

package exastencils.base.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l4._
import exastencils.core._
import exastencils.field.l3._
import exastencils.logger.Logger
import exastencils.optimization.l3._
import exastencils.prettyprinting._

/// L3_Equation

case class L3_Equation(var lhs : L3_Expression, var rhs : L3_Expression) extends L3_Node with PrettyPrintable with L3_Progressable {
  override def prettyprint(out : PpStream) = out << lhs << " == " << rhs
  override def progress = ProgressLocation(L4_Equation(lhs.progress, rhs.progress))

  def asZeroEquation() : L3_Expression = {
    val zeroEq : L3_Expression = Duplicate(lhs - rhs)
    L3_GeneralSimplifyWrapper.process(zeroEq)
  }

  def splitLhsRhs(unknowns : ListBuffer[L3_Field]) = {
    var newLhs : L3_Expression = 0.0
    var newRhs : L3_Expression = 0.0

    def insertExpr(e : L3_Expression, origFromLhs : Boolean) = {
      val containsUnknown = StateManager.findFirst({
        fieldAccess : L3_FieldAccess => unknowns.contains(fieldAccess.target) // TODO: filter by slot?
      }, L3_ExpressionStatement(e)).isDefined

      if (containsUnknown)
        newLhs += (if (origFromLhs) e else L3_Negative(e))
      else
        newRhs += (if (origFromLhs) L3_Negative(e) else e)
    }

    def process(e : L3_Expression, origFromLhs : Boolean) : Unit = {
      e match {
        case add : L3_Addition => add.summands.foreach(insertExpr(_, origFromLhs))

        case sub : L3_Subtraction =>
          process(sub.left, origFromLhs)
          process(sub.right, !origFromLhs)

        case _ : L3_Multiplication | _ : L3_Access | _ : L3_ConstantExpression => insertExpr(e, origFromLhs)

        case o =>
          Logger.warn(s"Found node with unsupported type when processing equation. The node is $o")
          insertExpr(o, origFromLhs)
      }
    }

    L3_FlattenComputation.doUntilDoneStandalone(lhs)
    L3_FlattenComputation.doUntilDoneStandalone(rhs)

    process(lhs, true)
    process(rhs, false)

    lhs = newLhs
    rhs = newRhs
  }
}
