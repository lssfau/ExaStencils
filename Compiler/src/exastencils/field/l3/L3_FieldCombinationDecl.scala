package exastencils.field.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l3._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_FieldCombinationDecl

object L3_FieldCombinationDecl {
  def apply(name : String, levels : Option[L3_LevelSpecification], combinationType : String, fields : List[L3_UnresolvedAccess]) =
    new L3_FieldCombinationDecl(name, levels, combinationType, fields.map(_.asInstanceOf[L3_Access]).to[ListBuffer])
}

case class L3_FieldCombinationDecl(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var combinationType : String,
    var fields : ListBuffer[L3_Access]) extends L3_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "FieldCombination " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " : " << combinationType << " = " << fields.map(_.name).mkString(", ")
  }

  override def addToKnowledge() : Unit = {
    L3_FieldCombinationCollection.add(
      L3_FieldCombination(name, levels.get.resolveLevel, combinationType, fields.map(_.asInstanceOf[L3_FieldAccess].target)))
  }

  override def progress = Logger.error(s"Trying to progress l3 field combination declaration for $name; this is not supported")
}

/// L3_ProcessFieldCombinationDeclarations

object L3_ProcessFieldCombinationDeclarations extends DefaultStrategy("Integrate L3 field combination declarations with knowledge") {
  this += Transformation("Process field combination declarations", {
    case decl : L3_FieldCombinationDecl if L3_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
