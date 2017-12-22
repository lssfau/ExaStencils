package exastencils.layoutTransformation.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.layoutTransformation.ir.IR_LayoutTransformationCollection
import exastencils.prettyprinting._

/// L4_LayoutSection

case class L4_LayoutSection(var statements : ListBuffer[L4_LayoutTransformStatement] = ListBuffer()) extends L4_Node with PrettyPrintable with L4_Progressable {
  override def prettyprint(out : PpStream) : Unit = {
    if (statements.nonEmpty) out << "LayoutTransformations {\n" <<< (statements, "\n") << "\n}"
  }

  override def progress = ProgressLocation(IR_LayoutTransformationCollection(statements.map(_.progress)))
}

/// L4_UnifyGlobalSections

object L4_UnifyLayoutSections extends DefaultStrategy("Unify all layout sections and ensure at least one section exists") {
  var unifiedLayoutSection = L4_LayoutSection()

  override def apply(applyAtNode : Option[Node]) : Unit = {
    // collect information
    super.apply(applyAtNode)

    // add collected info to root
    ExaRootNode.l4_root.nodes = unifiedLayoutSection +: ExaRootNode.l4_root.nodes

    // reset unifiedLayoutSection for potential subsequent runs
    unifiedLayoutSection = L4_LayoutSection()
  }

  this += new Transformation("Collect and consume layout sections", {
    case layouts : L4_LayoutSection =>
      unifiedLayoutSection.statements ++= layouts.statements

      None
  })
}
