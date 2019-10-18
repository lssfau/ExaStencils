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

package exastencils.base.l1

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.prettyprinting._

/// L1_FunctionCall

object L1_FunctionCall {
  def apply(function : L1_FunctionReference, args : L1_Expression*) =
    new L1_FunctionCall(function, args.to[ListBuffer])
}

case class L1_FunctionCall(var function : L1_FunctionReference, var arguments : ListBuffer[L1_Expression]) extends L1_Expression {
  def prettyprint(out : PpStream) = { out << function << " ( " <<< (arguments, ", ") << " )" }
  override def progress = ProgressLocation(L2_FunctionCall(function.progress, arguments.map(_.progress)))
  def name = function.name
}
