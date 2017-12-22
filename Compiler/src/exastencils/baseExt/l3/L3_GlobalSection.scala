package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_GlobalSection
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L3_GlobalSection

object L3_GlobalSection {
  def apply() = new L3_GlobalSection(ListBuffer())
  def apply(declarations : List[L3_VariableDeclaration]) : L3_GlobalSection = apply(declarations.to[ListBuffer])
}

case class L3_GlobalSection(var declarations : ListBuffer[L3_VariableDeclaration]) extends L3_Node with PrettyPrintable with L3_Progressable {
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
