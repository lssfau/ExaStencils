package exastencils.waLBerla.l4.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Node
import exastencils.base.l4.L4_Progressable
import exastencils.base.l4.L4_Statement
import exastencils.base.l4.L4_VariableDeclaration
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.prettyprinting.PrettyPrintable
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaCollection

/// L4_WaLBerlaVarsSection

object L4_WaLBerlaVarsSection {
  def apply(declarations : List[L4_Statement]) : L4_WaLBerlaVarsSection = apply(declarations.to[ListBuffer])
}

case class L4_WaLBerlaVarsSection(var declarations : ListBuffer[L4_Statement] = ListBuffer()) extends L4_Node with PrettyPrintable with L4_Progressable {
  override def prettyprint(out : PpStream) = {
    if (declarations.nonEmpty) out << "WaLBerlaVars {\n" <<< (declarations, "\n") << "\n}"
  }

  override def progress = ProgressLocation(IR_WaLBerlaCollection(declarations.map {
    case decl : L4_VariableDeclaration => decl.progress
    case other                         => Logger.error(s"Found unsupported node type in L4_WaLBerlaVarsSection: ${ other.getClass.getTypeName }")
  }))
}

/// L4_UnifyWaLBerlaVarsSections

object L4_UnifyWaLBerlaVarsSections extends DefaultStrategy("Unify all WaLBerlaVars sections and ensure at least one section exists") {
  var unifiedSection = L4_WaLBerlaVarsSection()

  override def apply(applyAtNode : Option[Node]) = {
    // collect information
    super.apply(applyAtNode)

    // add collected info to root
    ExaRootNode.l4_root.nodes = unifiedSection +: ExaRootNode.l4_root.nodes

    // reset unifiedGlobalSection for potential subsequent runs
    unifiedSection = L4_WaLBerlaVarsSection()
  }

  this += new Transformation("Collect and consume WaLBerlaVars sections", {
    case vars : L4_WaLBerlaVarsSection =>
      unifiedSection.declarations ++= vars.declarations
      None
  })
}
