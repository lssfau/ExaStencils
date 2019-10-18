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

package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.field.l3._
import exastencils.operator.l4.L4_OperatorTimesField
import exastencils.prettyprinting.PpStream

// TODO: is it really necessary to wrap convolutions in separate nodes?

/// L3_OperatorTimesField

case class L3_OperatorTimesField(var lhs : L3_OperatorAccess, var rhs : L3_FieldAccess) extends L3_Expression {
  def prettyprint(out : PpStream) = out << lhs << " * " << rhs
  override def progress = ProgressLocation(L4_OperatorTimesField(lhs.progress, rhs.progress))
}

/// L3_ResolveOperatorTimesField

object L3_ResolveOperatorTimesField extends DefaultStrategy("Resolving L3 operator field convolutions") {
  this += new Transformation("Resolve", {
    case mult @ L3_Multiplication(factors) =>
      val newFactors = ListBuffer[L3_Expression]()
      var skipNext = false
      for (i <- factors.indices) factors(i) match {
        case _ if skipNext => skipNext = false

        case op : L3_OperatorAccess =>
          if (i + 1 < factors.indices.length && factors(i + 1).isInstanceOf[L3_FieldAccess]) {
            newFactors += L3_OperatorTimesField(op, factors(i + 1).asInstanceOf[L3_FieldAccess])
            skipNext = true
          } else {
            newFactors += op
          }

        case other => newFactors += other
      }

      if (newFactors.length != factors.length) {
        L3_Multiplication(newFactors)
      } else {
        mult
      }
  })
}
