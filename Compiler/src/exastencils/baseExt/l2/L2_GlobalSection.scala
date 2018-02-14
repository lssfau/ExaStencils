package exastencils.baseExt.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.baseExt.l3.L3_GlobalSection
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L2_GlobalSection

object L2_GlobalSection {
  def apply() = new L2_GlobalSection(ListBuffer())
  def apply(declarations : List[L2_Statement]) : L2_GlobalSection = apply(declarations.to[ListBuffer])
}

case class L2_GlobalSection(var declarations : ListBuffer[L2_Statement]) extends L2_Node with PrettyPrintable with L2_Progressable {
  override def prettyprint(out : PpStream) = {
    if (declarations.nonEmpty) out << "Globals {\n" <<< (declarations, "\n") << "\n}"
  }

  override def progress = ProgressLocation(L3_GlobalSection(declarations.map(_.progress)))
}

/// L2_UnifyGlobalSections

object L2_UnifyGlobalSections extends DefaultStrategy("Unify all global sections and ensure at least one section exists") {
  var unifiedGlobalSection = L2_GlobalSection()

  override def apply(applyAtNode : Option[Node]) = {
    // collect information
    super.apply(applyAtNode)

    // add collected info to root
    ExaRootNode.l2_root.nodes = unifiedGlobalSection +: ExaRootNode.l2_root.nodes

    // reset unifiedGlobalSection for potential subsequent runs
    unifiedGlobalSection = L2_GlobalSection()
  }

  this += new Transformation("Collect and consume global sections", {
    case globals : L2_GlobalSection =>
      unifiedGlobalSection.declarations ++= globals.declarations

      None
  })
}
