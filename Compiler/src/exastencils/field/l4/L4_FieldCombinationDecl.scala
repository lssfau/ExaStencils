package exastencils.field.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.knowledge.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_FieldCombinationDecl

case class L4_FieldCombinationDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var combinationType : String,
    var fields : ListBuffer[L4_Access]) extends L4_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "FieldCombination " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " : " << combinationType << " = " << fields.map(_.name).mkString(", ")
  }

  override def addToKnowledge() : Unit = {
    L4_FieldCombinationCollection.add(
      L4_FieldCombination(name, levels.get.resolveLevel, combinationType, fields.map(_.asInstanceOf[L4_FieldAccess].target)))
  }

  override def progress = Logger.error(s"Trying to progress l4 field combination declaration for $name; this is not supported")
}

/// L4_ProcessFieldCombinationDeclarations

object L4_ProcessFieldCombinationDeclarations extends DefaultStrategy("Integrate L4 field combination declarations with knowledge") {
  this += Transformation("Process field combination declarations", {
    case decl : L4_FieldCombinationDecl if L4_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
