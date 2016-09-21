package exastencils.interfacing.l4

import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.field.l4._
import exastencils.knowledge.l4.L4_KnowledgeDecl
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_ExternalFieldDecl

case class L4_ExternalFieldDecl(
    var identifier : String,
    var fieldLayout : String,
    var targetField : Access) extends L4_KnowledgeDecl {

  override def prettyprint(out : PpStream) = out << "external Field " << identifier << " <" << fieldLayout << "> => " << targetField << '\n'

  override def addToKnowledge() = {
    val resolvedAccess = targetField match {
      case access : UnresolvedAccess =>
        if (access.dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on field - was an offset access (@) intended?")
        L4_FieldAccess(access.name, access.level.get.resolveLevel,
          access.slot.getOrElse(L4_ActiveSlot), access.arrayIndex, access.offset)
      case access : L4_FieldAccess   => access
    }
    val resolvedField = resolvedAccess.target
    val resolvedFieldLayout = resolvedField.fieldLayout
    L4_ExternalField(identifier, resolvedField.level, resolvedFieldLayout, resolvedField)
  }
}

/// L4_ProcessExternalFieldDeclarations

object L4_ProcessExternalFieldDeclarations extends DefaultStrategy("Integrating L4 external field declarations with knowledge") {
  this += Transformation("Process new external fields", {
    case externalFieldDecl : L4_ExternalFieldDecl =>
      externalFieldDecl.addToKnowledge()
      None // consume declaration statement
  })
}