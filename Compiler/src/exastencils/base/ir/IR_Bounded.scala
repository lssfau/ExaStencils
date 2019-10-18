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

package exastencils.base.ir

import exastencils.datastructures._
import exastencils.prettyprinting.PpStream

/// IR_BoundedScalar

case class IR_BoundedScalar(var min : Long, var max : Long, var expr : IR_Expression) extends IR_Expression {
  override def datatype = expr.datatype
  override def prettyprint(out : PpStream) : Unit = out << expr

  def expandSpecial() = expr
}

/// IR_ResolveBoundedScalar

object IR_ResolveBoundedScalar extends DefaultStrategy("Resolve BoundedScalar nodes") {
  this += new Transformation("Resolve", {
    case index : IR_BoundedScalar => index.expandSpecial()
  })
}
