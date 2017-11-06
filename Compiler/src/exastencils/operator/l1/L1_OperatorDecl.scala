package exastencils.operator.l1

import exastencils.base.l1._
import exastencils.datastructures._
import exastencils.knowledge.l1._
import exastencils.logger._
import exastencils.prettyprinting.PpStream

/// L1_OperatorDecl

case class L1_OperatorDecl(
    var name : String,
    var levels : Option[L1_LevelSpecification],
    var expr : L1_Expression) extends L1_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "Operator " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " = " << expr
  }

  override def addToKnowledge() : Unit = {
    val level = L1_LevelSpecification.asSingleLevel(levels)
    L1_OperatorCollection.add(L1_Operator(name, level, expr))
  }

  override def progress = Logger.error(s"Trying to progress L1 operator declaration for operator $name; this is not supported")
}

/// L1_PrepareOperatorDeclaration

object L1_PrepareOperatorDeclarations extends DefaultStrategy("Prepare knowledge for L1 operators") {
  this += Transformation("Process new operators", {
    case decl : L1_OperatorDecl =>
      L1_OperatorCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L1_ProcessOperatorDeclaration

object L1_ProcessOperatorDeclarations extends DefaultStrategy("Integrate L1 operator declarations with knowledge") {
  this += Transformation("Process new operators", {
    case decl : L1_OperatorDecl if L1_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
