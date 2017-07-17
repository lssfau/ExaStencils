package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_GlobalSection
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L3_GlobalSection

object L3_GlobalSection {
  def apply() = new L3_GlobalSection(ListBuffer(), ListBuffer())

  def apply(valueDeclarations : List[L3_ValueDeclaration], variableDeclarations : List[L3_VariableDeclaration]) =
    new L3_GlobalSection(valueDeclarations.to[ListBuffer], variableDeclarations.to[ListBuffer])

  def apply(mixedDeclarations : List[L3_Statement]) : L3_GlobalSection = apply(mixedDeclarations.to[ListBuffer])

  def apply(mixedDeclarations : ListBuffer[L3_Statement]) = {
    val (valDecl, varDecl) = mixedDeclarations.partition(_.isInstanceOf[L3_ValueDeclaration])
    new L3_GlobalSection(valDecl.map(_.asInstanceOf[L3_ValueDeclaration]), varDecl.map(_.asInstanceOf[L3_VariableDeclaration]))
  }
}

case class L3_GlobalSection(
    var valueDeclarations : ListBuffer[L3_ValueDeclaration],
    var variableDeclarations : ListBuffer[L3_VariableDeclaration]) extends L3_Node with PrettyPrintable with L3_Progressable {

  override def prettyprint(out : PpStream) = {
    out << "Globals {\n"
    out <<< (valueDeclarations, "\n") << "\n"
    out <<< (variableDeclarations, "\n")
    out << "\n}\n"
  }

  override def progress = L4_GlobalSection(valueDeclarations.map(_.progress), variableDeclarations.map(_.progress))
}

/// L3_UnifyGlobalSections

object L3_UnifyGlobalSections extends DefaultStrategy("Unify all global sections and ensure at least one section exists") {
  var unifiedGlobalSection = L3_GlobalSection()

  override def apply(applyAtNode : Option[Node]) = {
    // collect information
    super.apply(applyAtNode)

    // add collected info to root
    ExaRootNode.l3_root.nodes += unifiedGlobalSection

    // reset unifiedGlobalSection for potential subsequent runs
    unifiedGlobalSection = L3_GlobalSection()
  }

  this += new Transformation("Collect and consume global sections", {
    case globals : L3_GlobalSection =>
      unifiedGlobalSection.valueDeclarations ++= globals.valueDeclarations
      unifiedGlobalSection.variableDeclarations ++= globals.variableDeclarations
      None
  })
}
