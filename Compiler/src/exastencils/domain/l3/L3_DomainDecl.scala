package exastencils.domain.l3

import exastencils.datastructures._
import exastencils.knowledge.l3.L3_KnowledgeDecl
import exastencils.logger._

/// L3_DomainDecl

abstract class L3_DomainDecl extends L3_KnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress L3 domain declaration for domain $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L3_PrepareDomainDeclarations

object L3_PrepareDomainDeclarations extends DefaultStrategy("Prepare knowledge for L3 domains") {
  this += Transformation("Process new domains", {
    case decl : L3_DomainDecl =>
      L3_DomainCollection.addDeclared(decl.name)
      decl // preserve declaration statement
  })
}

/// L3_ProcessDomainDeclarations

object L3_ProcessDomainDeclarations extends DefaultStrategy("Integrate L3 domain declarations with knowledge") {
  this += Transformation("Process new domains", {
    case decl : L3_DomainDecl =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
