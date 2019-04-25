package exastencils.field.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.l1._
import exastencils.baseExt.l1.L1_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l1._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_FieldCombinationDecl

object L1_FieldCombinationDecl {
  def apply(name : String, levels : Option[L1_LevelSpecification], combinationType : String, fields : List[L1_UnresolvedAccess]) =
    new L1_FieldCombinationDecl(name, levels, combinationType, fields.map(_.asInstanceOf[L1_Access]).to[ListBuffer])
}

case class L1_FieldCombinationDecl(
    var name : String,
    var levels : Option[L1_LevelSpecification],
    var combinationType : String,
    var fields : ListBuffer[L1_Access]) extends L1_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "FieldCombination " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " : \"" << combinationType << "\" = " << fields.map(_.name).mkString(", ")
  }

  override def addToKnowledge() : Unit = {
    L1_FieldCombinationCollection.add(
      L1_FieldCombination(name, levels.get.resolveLevel, combinationType, fields.map(_.asInstanceOf[L1_FieldAccess].target)))
  }

  override def progress = Logger.error(s"Trying to progress l1 field combination declaration for $name; this is not supported")
}

/// L1_ProcessFieldCombinationDeclarations

object L1_ProcessFieldCombinationDeclarations extends DefaultStrategy("Integrate L1 field combination declarations with knowledge") {
  this += Transformation("Process field combination declarations", {
    case decl : L1_FieldCombinationDecl if L1_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}