package exastencils.field.l2

import exastencils.datastructures._
import exastencils.knowledge.l2._
import exastencils.logger._

/// L2_FieldDecl

abstract class L2_FieldDecl extends L2_LeveledKnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress L2 field declaration for field $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L2_PrepareFieldDeclaration

object L2_PrepareFieldDeclarations extends DefaultStrategy("Prepare knowledge for L2 fields") {
  this += Transformation("Process new fields", {
    case decl : L2_FieldDecl =>
      L2_FieldCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L2_ProcessFieldDeclarations

object L2_ProcessFieldDeclarations extends DefaultStrategy("Integrate L2 field declarations with knowledge") {
  this += Transformation("Process field declarations", {
    case decl : L2_FieldDecl if !L2_FutureKnowledgeAccess.existsInStmt(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
