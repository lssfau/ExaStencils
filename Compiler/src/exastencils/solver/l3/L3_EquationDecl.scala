package exastencils.solver.l3

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.knowledge.l3.L3_LeveledKnowledgeDecl
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_EquationDecl

case class L3_EquationDecl(
    var name : String, var levels : Option[L3_LevelSpecification],
    var lhs : L3_Expression, var rhs : L3_Expression) extends L3_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"

  override def addToKnowledge() : Unit = {
    L3_EquationCollection.add(
      L3_NamedEquation(name, L3_LevelSpecification.asSingleLevel(levels), L3_Equation(lhs, rhs)))
  }

  override def progress = Logger.error(s"Trying to progress L3 equation declaration for equation $name; this is not supported")
}

/// L3_PrepareEquationDeclaration

object L3_PrepareEquationDeclarations extends DefaultStrategy("Prepare knowledge for L3 equations") {
  this += Transformation("Process new equations", {
    case decl : L3_EquationDecl =>
      L3_EquationCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L3_ProcessEquationDeclarations

object L3_ProcessEquationDeclarations extends DefaultStrategy("Integrate L3 equation declarations with knowledge") {
  this += Transformation("Process equation declarations", {
    case decl : L3_EquationDecl if L3_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
