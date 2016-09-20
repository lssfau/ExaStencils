package exastencils.interfacing.l4

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.field.l4.L4_FieldAccess
import exastencils.prettyprinting.PpStream

/// L4_ExternalFieldDecl

case class L4_ExternalFieldDecl(
    var identifier : String,
    var fieldLayout : String,
    var targetField : Access
) extends L4_KnowledgeDeclStatement {
  override def prettyprint(out : PpStream) = out << "external Field " << identifier << " <" << fieldLayout << "> => " << targetField << '\n'

  override def addToKnowledge() = {
    val resolvedAccess = targetField match {
      case access : UnresolvedAccess => access.resolveToFieldAccess
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
