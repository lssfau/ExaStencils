package exastencils.interfacing.l4

import exastencils.base.l4.L4_Access
import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.knowledge.l4._
import exastencils.prettyprinting.PpStream

/// L4_ExternalFieldDecl

case class L4_ExternalFieldDecl(
    var name : String,
    var fieldLayout : String,
    var targetField : L4_Access) extends L4_KnowledgeDecl {

  override def prettyprint(out : PpStream) = out << "external Field " << name << " <" << fieldLayout << "> => " << targetField << '\n'

  override def addToKnowledge() : Unit = {
    val resolvedField = targetField.asInstanceOf[L4_FieldAccess].target
    val resolvedFieldLayout = resolvedField.fieldLayout
    val extField = L4_ExternalField(name, resolvedField.level, resolvedFieldLayout, resolvedField)
    L4_ExternalFieldCollection.add(extField)
  }

  override def progress = ???
}

/// L4_ProcessExternalFieldDeclarations

object L4_ProcessExternalFieldDeclarations extends DefaultStrategy("Integrating L4 external field declarations with knowledge") {
  this += Transformation("Process new external fields", {
    case externalFieldDecl : L4_ExternalFieldDecl =>
      externalFieldDecl.addToKnowledge()
      None // consume declaration statement
  })
}
