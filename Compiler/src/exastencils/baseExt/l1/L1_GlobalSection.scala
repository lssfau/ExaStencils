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

package exastencils.baseExt.l1

import scala.collection.mutable.ListBuffer

import exastencils.base._
import exastencils.base.l1._
import exastencils.base.l2.L2_Statement
import exastencils.baseExt.l2.L2_GlobalSection
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L1_GlobalSection

object L1_GlobalSection {
  def apply() = new L1_GlobalSection(ListBuffer())
  def apply(declarations : List[L1_VariableDeclaration]) : L1_GlobalSection = apply(declarations.to[ListBuffer])
}

case class L1_GlobalSection(var declarations : ListBuffer[L1_VariableDeclaration]) extends L1_Node with PrettyPrintable with L1_Progressable {
  override def prettyprint(out : PpStream) = {
    if (declarations.nonEmpty) out << "Globals {\n" <<< (declarations, "\n") << "\n}"
  }

  override def progress = ProgressLocation(L2_GlobalSection(declarations.map(_.progress : L2_Statement)))
}

/// L1_UnifyGlobalSections

object L1_UnifyGlobalSections extends DefaultStrategy("Unify all global sections and ensure at least one section exists") {
  var unifiedGlobalSection = L1_GlobalSection()

  override def apply(applyAtNode : Option[Node]) = {
    // collect information
    super.apply(applyAtNode)

    // add collected info to root
    ExaRootNode.l1_root.nodes = unifiedGlobalSection +: ExaRootNode.l1_root.nodes

    // reset unifiedGlobalSection for potential subsequent runs
    unifiedGlobalSection = L1_GlobalSection()
  }

  this += new Transformation("Collect and consume global sections", {
    case globals : L1_GlobalSection =>
      unifiedGlobalSection.declarations ++= globals.declarations

      None
  })
}
