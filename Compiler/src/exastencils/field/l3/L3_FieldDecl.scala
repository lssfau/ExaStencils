package exastencils.field.l3

import exastencils.datastructures._
import exastencils.knowledge.l3._
import exastencils.logger._

/// L3_FieldDecl

abstract class L3_FieldDecl extends L3_LeveledKnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress l3 field declaration for field $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L3_PrepareFieldDeclaration

object L3_PrepareFieldDeclarations extends DefaultStrategy("Prepare knowledge for L3 fields") {
  this += Transformation("Process new fields", {
    case decl : L3_FieldDecl =>
      L3_FieldCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L3_ProcessFieldDeclarations

object L3_ProcessFieldDeclarations extends DefaultStrategy("Integrate L3 field declarations with knowledge") {
  this += Transformation("Process field declarations", {
    case decl : L3_FieldDecl if !L3_FutureKnowledgeAccess.existsInStmt(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
