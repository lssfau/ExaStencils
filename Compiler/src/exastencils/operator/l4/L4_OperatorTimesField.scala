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

package exastencils.operator.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.fieldlike.l4.L4_FieldLikeAccess
import exastencils.operator.ir.IR_OperatorTimesField
import exastencils.prettyprinting.PpStream

/// L4_OperatorTimesField

case class L4_OperatorTimesField(var lhs : L4_OperatorAccess, var rhs : L4_FieldLikeAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = out << lhs << " * " << rhs
  override def progress = ProgressLocation(IR_OperatorTimesField(lhs.progress, rhs.progress))
}

/// L4_ResolveOperatorTimesField

object L4_ResolveOperatorTimesField extends DefaultStrategy("Resolving L4 operator field convolutions") {
  this += new Transformation("Resolve", {
    case mult @ L4_Multiplication(factors) =>
      val newFactors = ListBuffer[L4_Expression]()
      var skipNext = false
      for (i <- factors.indices) factors(i) match {
        case _ if skipNext => skipNext = false

        case op : L4_OperatorAccess =>
          if (i + 1 < factors.indices.length && factors(i + 1).isInstanceOf[L4_FieldLikeAccess]) {
            newFactors += L4_OperatorTimesField(op, factors(i + 1).asInstanceOf[L4_FieldLikeAccess])
            skipNext = true
          } else {
            newFactors += op
          }

        case other => newFactors += other
      }

      if (newFactors.length != factors.length) {
        L4_Multiplication(newFactors)
      } else {
        mult
      }
  })
}

/// L4_UnresolveOperatorTimesField

object L4_UnresolveOperatorTimesField extends DefaultStrategy("Revert stencil field convolutions to plain multiplications") {
  this += new Transformation("Replace", {
    case L4_OperatorTimesField(lhs, rhs) => L4_Multiplication(lhs, rhs)
  })
}
