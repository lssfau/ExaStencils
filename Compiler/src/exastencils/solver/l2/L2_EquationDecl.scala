package exastencils.solver.l2

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.knowledge.l2.L2_LeveledKnowledgeDecl
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_EquationDecl

case class L2_EquationDecl(
    var name : String, var levels : Option[L2_LevelSpecification], equation : L2_Equation) extends L2_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"
  override def addToKnowledge() = L2_EquationCollection.add(L2_NamedEquation(name, L2_LevelSpecification.asSingleLevel(levels), equation))
  override def progress = Logger.error(s"Trying to progress L2 equation declaration for equation $name; this is not supported")
}

/// L2_PrepareEquationDeclaration

object L2_PrepareEquationDeclarations extends DefaultStrategy("Prepare knowledge for L2 equations") {
  this += Transformation("Process new equations", {
    case decl : L2_EquationDecl =>
      L2_EquationCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L2_ProcessEquationDeclarations

object L2_ProcessEquationDeclarations extends DefaultStrategy("Integrate L2 equation declarations with knowledge") {
  this += Transformation("Process equation declarations", {
    case decl : L2_EquationDecl if L2_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
