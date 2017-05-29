package exastencils.field.l4

import exastencils.datastructures._
import exastencils.knowledge.l4._
import exastencils.logger._

/// L4_FieldDecl

object L4_FieldDecl {
  var runningIndex = 0
}

abstract class L4_FieldDecl extends L4_LeveledKnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress l4 field declaration for field $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L4_PrepareFieldDeclaration

object L4_PrepareFieldDeclarations extends DefaultStrategy("Prepare knowledge for L4 fields") {
  this += Transformation("Process new fields", {
    case decl : L4_FieldDecl =>
      L4_FieldCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L4_ProcessFieldDeclarations

object L4_ProcessFieldDeclarations extends DefaultStrategy("Integrate L4 field declarations with knowledge") {
  this += Transformation("Process field declarations", {
    case decl : L4_FieldDecl if !L4_FutureKnowledgeAccess.existsInStmt(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
