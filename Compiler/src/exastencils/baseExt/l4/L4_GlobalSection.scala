package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base._
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_GlobalSection

object L4_GlobalSection {
  def apply() = new L4_GlobalSection(ListBuffer())
  def apply(declarations : List[L4_Statement]) : L4_GlobalSection = apply(declarations.to[ListBuffer])
}

case class L4_GlobalSection(var declarations : ListBuffer[L4_Statement]) extends L4_Node with PrettyPrintable with L4_Progressable {
  override def prettyprint(out : PpStream) = {
    if (declarations.nonEmpty) out << "Globals {\n" <<< (declarations, "\n") << "\n}"
  }

  override def progress = ProgressLocation(IR_GlobalCollection(declarations.map {
    case decl : L4_VariableDeclaration => decl.progress
    case other                         => Logger.error(s"Found unsupported node type in L4_GlobalSelection: ${ other.getClass.getTypeName }")
  }))
}

/// L4_UnifyGlobalSections

object L4_UnifyGlobalSections extends DefaultStrategy("Unify all global sections and ensure at least one section exists") {
  var unifiedGlobalSection = L4_GlobalSection()

  override def apply(applyAtNode : Option[Node]) = {
    // collect information
    super.apply(applyAtNode)

    // add collected info to root
    ExaRootNode.l4_root.nodes = unifiedGlobalSection +: ExaRootNode.l4_root.nodes

    // reset unifiedGlobalSection for potential subsequent runs
    unifiedGlobalSection = L4_GlobalSection()
  }

  this += new Transformation("Collect and consume global sections", {
    case globals : L4_GlobalSection =>
      unifiedGlobalSection.declarations ++= globals.declarations

      None
  })
}
