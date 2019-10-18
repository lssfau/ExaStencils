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
import exastencils.base.l2.L2_Statement
import exastencils.baseExt.l3._
import exastencils.prettyprinting.PpStream
import exastencils.util.l2.L2_GeneralParameter

/// L2_ApplicationHints

object L2_ApplicationHints {
  def apply(hints : List[L2_ApplicationHint]) = new L2_ApplicationHints(hints.to[ListBuffer])
}

case class L2_ApplicationHints(var hints : ListBuffer[L2_ApplicationHint]) extends L2_Statement {
  override def prettyprint(out : PpStream) = out << "ApplicationHint {\n" <<< (hints, "\n") << "\n}"
  override def progress = ProgressLocation(L3_ApplicationHints(hints.map(_.progress)))
}

/// L2_ApplicationHint

abstract class L2_ApplicationHint extends L2_Statement {
  override def progress : L3_ApplicationHint
}

/// L2_ApplicationParameter

case class L2_ApplicationParameter(var name : String, var value : Any) extends L2_ApplicationHint with L2_GeneralParameter {
  override def prettyprint(out : PpStream) = out << name << " = " << printVal
  override def progress = ProgressLocation(L3_ApplicationParameter(name, value))
}
