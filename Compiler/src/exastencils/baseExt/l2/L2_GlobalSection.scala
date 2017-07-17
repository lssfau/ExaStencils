package exastencils.baseExt.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.l2._
import exastencils.baseExt.l3.L3_GlobalSection
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L2_GlobalSection

object L2_GlobalSection {
  def apply() = new L2_GlobalSection(ListBuffer(), ListBuffer())

  def apply(valueDeclarations : List[L2_ValueDeclaration], variableDeclarations : List[L2_VariableDeclaration]) =
    new L2_GlobalSection(valueDeclarations.to[ListBuffer], variableDeclarations.to[ListBuffer])

  def apply(mixedDeclarations : List[L2_Statement]) : L2_GlobalSection = apply(mixedDeclarations.to[ListBuffer])

  def apply(mixedDeclarations : ListBuffer[L2_Statement]) = {
    val (valDecl, varDecl) = mixedDeclarations.partition(_.isInstanceOf[L2_ValueDeclaration])
    new L2_GlobalSection(valDecl.map(_.asInstanceOf[L2_ValueDeclaration]), varDecl.map(_.asInstanceOf[L2_VariableDeclaration]))
  }
}

case class L2_GlobalSection(
    var valueDeclarations : ListBuffer[L2_ValueDeclaration],
    var variableDeclarations : ListBuffer[L2_VariableDeclaration]) extends L2_Node with PrettyPrintable with L2_Progressable {

  override def prettyprint(out : PpStream) = {
    out << "Globals {\n"
    out <<< (valueDeclarations, "\n") << "\n"
    out <<< (variableDeclarations, "\n")
    out << "\n}\n"
  }

  override def progress = L3_GlobalSection(valueDeclarations.map(_.progress), variableDeclarations.map(_.progress))
}

/// L2_UnifyGlobalSections

object L2_UnifyGlobalSections extends DefaultStrategy("Unify all global sections and ensure at least one section exists") {
  var unifiedGlobalSection = L2_GlobalSection()

  override def apply(applyAtNode : Option[Node]) = {
    // collect information
    super.apply(applyAtNode)

    // add collected info to root
    ExaRootNode.l2_root.nodes += unifiedGlobalSection

    // reset unifiedGlobalSection for potential subsequent runs
    unifiedGlobalSection = L2_GlobalSection()
  }

  this += new Transformation("Collect and consume global sections", {
    case globals : L2_GlobalSection =>
      unifiedGlobalSection.valueDeclarations ++= globals.valueDeclarations
      unifiedGlobalSection.variableDeclarations ++= globals.variableDeclarations
      None
  })
}
