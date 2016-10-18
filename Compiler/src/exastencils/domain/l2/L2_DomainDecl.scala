package exastencils.domain.l2

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.logger._
import exastencils.prettyprinting._

case class L2_DomainDecl(var name : String) extends L2_Statement {
  override def prettyprint(out : PpStream) = out << "Domain" << name
  override def progress = { Logger.error(s"Trying to progress l2 domain declaration for domain $name; this is not supported") }
}

/// strategies

object L2_ProcessDomainDeclarations extends DefaultStrategy("Integrate Layer2 domain declarations with knowledge") {
  this += Transformation("Process new domains", {
    case domain : L2_DomainDecl =>
      L2_DomainCollection.add(L2_Domain(domain.name))
      None // consume declaration statement
  })
}
