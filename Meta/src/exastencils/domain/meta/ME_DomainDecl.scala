package exastencils.domain.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_DomainDecl extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/domain/|LAYER_LC|/|LAYER_UC|_DomainDecl.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.domain.|LAYER_LC|

import exastencils.datastructures._
import exastencils.knowledge.|LAYER_LC|.|LAYER_UC|_KnowledgeDecl
import exastencils.logger._
import exastencils.prettyprinting._

/// |LAYER_UC|_DomainDecl

case class |LAYER_UC|_DomainDecl(var name : String) extends |LAYER_UC|_KnowledgeDecl {
  override def prettyprint(out : PpStream) = out << "Domain" << name
  override def progress = Logger.error(s"Trying to progress |LAYER_UC| domain declaration for domain $name; this is not supported")
}

/// |LAYER_UC|_PrepareDomainDeclarations

object |LAYER_UC|_PrepareDomainDeclarations extends DefaultStrategy("Prepare knowledge for |LAYER_UC| domains") {
  this += Transformation("Process new domains", {
    case decl : |LAYER_UC|_DomainDecl =>
      |LAYER_UC|_DomainCollection.addDeclared(decl.name)
      decl // preserve declaration statement
  })
}

/// |LAYER_UC|_ProcessDomainDeclarations

object |LAYER_UC|_ProcessDomainDeclarations extends DefaultStrategy("Integrate |LAYER_UC| domain declarations with knowledge") {
  this += Transformation("Process new domains", {
    case domain : |LAYER_UC|_DomainDecl =>
      |LAYER_UC|_DomainCollection.add(|LAYER_UC|_Domain(domain.name))
      None // consume declaration statement
  })
}
"""
  }
}
