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

package exastencils.field.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.field.l4.L4_FieldFieldConvolution
import exastencils.prettyprinting.PpStream

/// L3_FieldFieldConvolution

case class L3_FieldFieldConvolution(var lhs : L3_FieldAccess, var rhs : L3_FieldAccess) extends L3_Expression {
  override def prettyprint(out : PpStream) = out << "dot ( " << lhs << ", " << rhs << " )"
  override def progress = ProgressLocation(L4_FieldFieldConvolution(lhs.progress, rhs.progress))
}

/// L3_ResolveFieldFieldConvolutions

object L3_ResolveFieldFieldConvolutions extends DefaultStrategy("Resolving L3 field field convolutions") {
  this += new Transformation("Resolve", {
    case fctCall : L3_FunctionCall if "dot" == fctCall.name =>
      fctCall.arguments match {
        case ListBuffer(lhs : L3_FieldAccess, rhs : L3_FieldAccess) => L3_FieldFieldConvolution(lhs, rhs)
        case _                                                      => fctCall
      }
  })
}
