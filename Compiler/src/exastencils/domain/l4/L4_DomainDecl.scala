package exastencils.domain.l4

import exastencils.datastructures._
import exastencils.knowledge.l4.L4_KnowledgeDecl
import exastencils.logger._

/// L4_DomainDecl

abstract class L4_DomainDecl extends L4_KnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress L4 domain declaration for domain $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L4_PrepareDomainDeclarations

object L4_PrepareDomainDeclarations extends DefaultStrategy("Prepare knowledge for L4 domains") {
  this += Transformation("Process new domains", {
    case decl : L4_DomainDecl =>
      L4_DomainCollection.addDeclared(decl.name)
      decl // preserve declaration statement
  })
}

/// L4_ProcessDomainDeclarations

object L4_ProcessDomainDeclarations extends DefaultStrategy("Integrate L4 domain declarations with knowledge") {
  this += Transformation("Process new domains", {
    case decl : L4_DomainDecl =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
