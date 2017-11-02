package exastencils.domain.l1

import exastencils.datastructures._
import exastencils.knowledge.l1.L1_KnowledgeDecl
import exastencils.logger._

/// L1_DomainDecl

abstract class L1_DomainDecl extends L1_KnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress L1 domain declaration for domain $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L1_PrepareDomainDeclarations

object L1_PrepareDomainDeclarations extends DefaultStrategy("Prepare knowledge for L1 domains") {
  this += Transformation("Process new domains", {
    case decl : L1_DomainDecl =>
      L1_DomainCollection.addDeclared(decl.name)
      decl // preserve declaration statement
  })
}

/// L1_ProcessDomainDeclarations

object L1_ProcessDomainDeclarations extends DefaultStrategy("Integrate L1 domain declarations with knowledge") {
  this += Transformation("Process new domains", {
    case decl : L1_DomainDecl =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
