package exastencils.domain.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_Domain extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/domain/|LAYER_LC|/|LAYER_UC|_Domain.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.domain.|LAYER_LC|

import exastencils.domain.|NEXT_LC|.|NEXT_UC|_Domain
import exastencils.knowledge.|LAYER_LC|.|LAYER_UC|_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// |LAYER_UC|_Domain

case class |LAYER_UC|_Domain(var name : String /* TODO: add other relevant information */) extends |LAYER_UC|_KnowledgeObject[|NEXT_UC|_Domain] {
  override def prettyprintDecl(out : PpStream) : Unit = ???
  override def progressImpl() = |NEXT_UC|_Domain(name)
}
"""
  }
}
