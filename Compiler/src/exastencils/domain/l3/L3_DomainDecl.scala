package exastencils.domain.l3

import exastencils.datastructures._
import exastencils.knowledge.l3.L3_KnowledgeDecl
import exastencils.logger._
import exastencils.prettyprinting._

/// L3_DomainDecl

case class L3_DomainDecl(var name : String) extends L3_KnowledgeDecl {
  override def prettyprint(out : PpStream) = out << "Domain" << name
  override def progress = Logger.error(s"Trying to progress L3 domain declaration for domain $name; this is not supported")
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
    case domain : L3_DomainDecl =>
      L3_DomainCollection.add(L3_Domain(domain.name))
      None // consume declaration statement
  })
}
