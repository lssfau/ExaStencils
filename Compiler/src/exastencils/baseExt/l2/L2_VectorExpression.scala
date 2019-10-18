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

package exastencils.baseExt.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.baseExt.l3.L3_VectorExpression
import exastencils.prettyprinting.PpStream

/// L2_VectorExpression

case class L2_VectorExpression(var entries : ListBuffer[L2_Expression], var rowVector : Boolean = false) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = ???
  override def progress = ProgressLocation(L3_VectorExpression(entries.map(_.progress), rowVector))
}
