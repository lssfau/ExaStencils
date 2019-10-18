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

package exastencils.util

import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.logger._

class CountNodes(id : String) extends DefaultStrategy("Count " + id) {
  var nodes = 0
  var annotations = 0
  this += Transformation("Count", {
    case x =>
      nodes += 1
      annotations += x.annotations.size
      x
  })

  override def apply(node : Option[Node]) = {
    nodes = 0
    super.apply(node)
    Logger.dbg(s"Counting $id resulted in $nodes nodes with $annotations annotations")
  }
}
