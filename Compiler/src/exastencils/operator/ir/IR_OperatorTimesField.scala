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

package exastencils.operator.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.prettyprinting.PpStream

/// IR_OperatorTimesField

case class IR_OperatorTimesField(var lhs : IR_OperatorAccess, var rhs : IR_FieldLikeAccess) extends IR_Expression {
  override def datatype = rhs.datatype
  override def prettyprint(out : PpStream) = out << lhs << " * " << rhs
}

/// IR_ResolveOperatorTimesField

object IR_ResolveOperatorTimesField extends DefaultStrategy("Resolving IR operator field convolutions") {
  this += new Transformation("Resolve", {
    case mult @ IR_Multiplication(factors) =>
      val newFactors = ListBuffer[IR_Expression]()
      var skipNext = false
      for (i <- factors.indices) factors(i) match {
        case _ if skipNext => skipNext = false

        case op : IR_OperatorAccess =>
          if (i + 1 < factors.indices.length && factors(i + 1).isInstanceOf[IR_FieldLikeAccess]) {
            newFactors += IR_OperatorTimesField(op, factors(i + 1).asInstanceOf[IR_FieldLikeAccess])
            skipNext = true
          } else {
            newFactors += op
          }

        case other => newFactors += other
      }

      if (newFactors.length != factors.length) {
        IR_Multiplication(newFactors)
      } else {
        mult
      }
  })
}

/// IR_UnresolveOperatorTimesField

object IR_UnresolveOperatorTimesField extends DefaultStrategy("Revert stencil field convolutions to plain multiplications") {
  this += new Transformation("Replace", {
    case IR_OperatorTimesField(lhs, rhs) => IR_Multiplication(lhs, rhs)
  })
}
