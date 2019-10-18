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

package exastencils.datastructures

import scala.util.parsing.input.Position

import exastencils.core.Duplicate

/// SourceLocation

object SourceLocation {
  Duplicate.registerImmutable(classOf[SourceLocation])
}

case class SourceLocation(var position : Option[Position] = None, var fileName : Option[String] = None) {
  // init companion object
  SourceLocation

  def toAppendString = {
    var s = ""

    if (position.isDefined)
      s += " at location " + position.get
    if (fileName.isDefined)
      s += " in file " + fileName.get
    if (position.isDefined)
      s += ":\n" + position.get.longString

    if (s.nonEmpty)
      s = " ;" + s

    s
  }
}
