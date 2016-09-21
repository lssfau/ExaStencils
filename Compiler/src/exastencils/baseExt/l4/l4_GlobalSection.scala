package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.globals.Globals
import exastencils.prettyprinting._

/// L4_GlobalSection

object L4_GlobalSection {
  def apply() = new L4_GlobalSection(ListBuffer(), ListBuffer())

  def apply(valueDeclarations : List[L4_ValueDeclaration], variableDeclarations : List[L4_VariableDeclaration]) =
    new L4_GlobalSection(valueDeclarations.to[ListBuffer], variableDeclarations.to[ListBuffer])

  def apply(mixedDeclarations : List[L4_Statement]) : L4_GlobalSection = apply(mixedDeclarations.to[ListBuffer])

  def apply(mixedDeclarations : ListBuffer[L4_Statement]) = {
    val valueDeclarations = mixedDeclarations.filter(_.isInstanceOf[L4_ValueDeclaration]).map(_.asInstanceOf[L4_ValueDeclaration])
    val variableDeclarations = mixedDeclarations.filter(_.isInstanceOf[L4_VariableDeclaration]).map(_.asInstanceOf[L4_VariableDeclaration])
    new L4_GlobalSection(valueDeclarations, variableDeclarations)
  }
}

case class L4_GlobalSection(
    var valueDeclarations : ListBuffer[L4_ValueDeclaration],
    var variableDeclarations : ListBuffer[L4_VariableDeclaration]) extends L4_Node with PrettyPrintable with L4_Progressable {

  override def prettyprint(out : PpStream) = {
    out << "Globals {\n"
    out <<< (valueDeclarations, "\n") << "\n"
    out <<< (variableDeclarations, "\n")
    out << "\n}\n"
  }

  // FIXME: unify multiple global sections
  override def progress = Globals(variableDeclarations.map(_.progress) ++ valueDeclarations.map(_.progress))
}

/// L4_UnifyGlobalSections

object L4_UnifyGlobalSections extends DefaultStrategy("Unify all global sections and ensure at least one section exists") {
  var unifiedGlobalSection = L4_GlobalSection()

  // FIXME: use transformation below instead of overriding apply -> requires ability to match root node
  override def apply(applyAtNode : Option[Node]) = {
    super.apply(applyAtNode)
    val root = StateManager.root.asInstanceOf[Root]
    root.otherNodes += unifiedGlobalSection
    // reset unifiedGlobalSection for potential subsequent runs
    unifiedGlobalSection = L4_GlobalSection()
  }

  this += new Transformation("Collect and consume global sections", {
    case globals : L4_GlobalSection =>
      unifiedGlobalSection.valueDeclarations ++= globals.valueDeclarations
      unifiedGlobalSection.variableDeclarations ++= globals.variableDeclarations
      None
  })

  //  this += new Transformation("Write back unified global section", {
  //    case root : Root =>
  //      root.otherNodes += unifiedGlobalSection
  //      // reset unifiedGlobalSection for potential subsequent runs
  //      unifiedGlobalSection = L4_GlobalSection()
  //      root
  //  })
}

/// L4_InlineGlobalValueDeclarations

object L4_InlineGlobalValueDeclarations extends DefaultStrategy("Propagate and inline global value declarations") {
  val globalVals = collection.mutable.HashMap[String, L4_Expression]()

  override def apply(applyAtNode : Option[Node]) = {
    globalVals.clear()
    super.apply(applyAtNode)
    globalVals.clear()
  }

  this += new Transformation("Collect global value declarations", {
    case globals : L4_GlobalSection =>
      globals.valueDeclarations.foreach(x => x.identifier match {
        case v : LeveledIdentifier => globalVals += ((v.name + "@@" + v.level, x.initialValue))
        case _                     => globalVals += ((x.identifier.name, x.initialValue))
      })
      globals
  })

  this += new Transformation("Resolve global values in expressions", {
    case x @ UnresolvedAccess(_, None, None, _, None, _)                        =>
      val value = globalVals.get(x.name)
      value match {
        case None => x // no hit
        case _    => Duplicate(value.get)
      }
    case x @ UnresolvedAccess(_, None, Some(L4_SingleLevel(level)), _, None, _) =>
      val value = globalVals.get(x.name + "@@" + level)
      value match {
        case None => x // no hit
        case _    => Duplicate(value.get)
      }
  })

  this += new Transformation("Remove propagated value declarations", {
    case globals : L4_GlobalSection =>
      globals.valueDeclarations.clear()
      globals
  })
}
