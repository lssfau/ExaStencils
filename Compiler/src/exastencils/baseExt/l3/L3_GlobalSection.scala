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

package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base._
import exastencils.base.l3._
import exastencils.base.l4.L4_Statement
import exastencils.baseExt.l4.L4_GlobalSection
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L3_GlobalSection

object L3_GlobalSection {
  def apply() = new L3_GlobalSection(ListBuffer())
  def apply(declarations : List[L3_Statement]) : L3_GlobalSection = apply(declarations.to[ListBuffer])
}

case class L3_GlobalSection(var declarations : ListBuffer[L3_Statement]) extends L3_Node with PrettyPrintable with L3_Progressable {
  override def prettyprint(out : PpStream) = {
    if (declarations.nonEmpty) out << "Globals {\n" <<< (declarations, "\n") << "\n}"
  }

  override def progress = ProgressLocation(L4_GlobalSection(declarations.map(_.progress)))
}

/// L3_UnifyGlobalSections

object L3_UnifyGlobalSections extends DefaultStrategy("Unify all global sections and ensure at least one section exists") {
  var unifiedGlobalSection = L3_GlobalSection()

  override def apply(applyAtNode : Option[Node]) = {
    // collect information
    super.apply(applyAtNode)

    // add collected info to root
    ExaRootNode.l3_root.nodes = unifiedGlobalSection +: ExaRootNode.l3_root.nodes

    // reset unifiedGlobalSection for potential subsequent runs
    unifiedGlobalSection = L3_GlobalSection()
  }

  this += new Transformation("Collect and consume global sections", {
    case globals : L3_GlobalSection =>
      unifiedGlobalSection.declarations ++= globals.declarations

      None
  })
}
