package exastencils.solver.l4

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_LeveledKnowledgeDecl
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_EquationDecl

case class L4_EquationDecl(
    var name : String, var levels : Option[L4_LevelSpecification],
    var lhs : L4_Expression, var rhs : L4_Expression) extends L4_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"

  override def addToKnowledge() : Unit = {
    L4_EquationCollection.add(
      L4_NamedEquation(name, L4_LevelSpecification.asSingleLevel(levels), L4_Equation(lhs, rhs)))
  }

  override def progress = Logger.error(s"Trying to progress L4 equation declaration for equation $name; this is not supported")
}

/// L4_PrepareEquationDeclaration

object L4_PrepareEquationDeclarations extends DefaultStrategy("Prepare knowledge for L4 equations") {
  this += Transformation("Process new equations", {
    case decl : L4_EquationDecl =>
      L4_EquationCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L4_ProcessEquationDeclarations

object L4_ProcessEquationDeclarations extends DefaultStrategy("Integrate L4 equation declarations with knowledge") {
  this += Transformation("Process equation declarations", {
    case decl : L4_EquationDecl if L4_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
