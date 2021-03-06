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

package exastencils.grid.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.grid.l4._
import exastencils.prettyprinting._

/// L3_Localization

object L3_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: sth more elegant?
  def dimToString(dim : Int) = ('x'.toInt + dim).toChar
  def stringToDim(dim : Char) = dim.toInt - 'x'.toInt

  // TODO: max number of dimensions?
  def availableLocalizations = List(L3_AtNode, L3_AtCellCenter, L3_AtFaceCenter(0), L3_AtFaceCenter(1), L3_AtFaceCenter(2), L3_AtBoundary)

  def resolve(name : String) = availableLocalizations.find(_.name.toLowerCase == name.toLowerCase).get
}

abstract class L3_Localization extends L3_Node with PrettyPrintable with L3_Progressable {
  def name : String
  override def prettyprint(out : PpStream) = out << name
  override def progress : L4_Localization
}

/// L3_AtNodes

case object L3_AtNode extends L3_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  def name = "Node"
  override def progress = ProgressLocation(L4_AtNode)
}

/// L3_AtCells

case object L3_AtCellCenter extends L3_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  def name = "Cell"
  override def progress = ProgressLocation(L4_AtCellCenter)
}

/// L3_AtFace

case class L3_AtFaceCenter(dim : Int) extends L3_Localization {
  def name = s"Face_${ L3_Localization.dimToString(dim) }"
  override def progress = ProgressLocation(L4_AtFaceCenter(dim))
}

/// L3_AtBoundary

// special object for accessing coordinates mapped to the boundary
case object L3_AtBoundary extends L3_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  def name = "Boundary"
  override def progress = ProgressLocation(L4_AtBoundary)
}
