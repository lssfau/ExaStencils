package exastencils.domain.l2

import exastencils.datastructures._
import exastencils.knowledge.l2.L2_KnowledgeDecl
import exastencils.logger._

/// L2_DomainDecl

abstract class L2_DomainDecl extends L2_KnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress L2 domain declaration for domain $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L2_PrepareDomainDeclarations

object L2_PrepareDomainDeclarations extends DefaultStrategy("Prepare knowledge for L2 domains") {
  this += Transformation("Process new domains", {
    case decl : L2_DomainDecl =>
      L2_DomainCollection.addDeclared(decl.name)
      decl // preserve declaration statement
  })
}

/// L2_ProcessDomainDeclarations

object L2_ProcessDomainDeclarations extends DefaultStrategy("Integrate L2 domain declarations with knowledge") {
  this += Transformation("Process new domains", {
    case decl : L2_DomainDecl =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
