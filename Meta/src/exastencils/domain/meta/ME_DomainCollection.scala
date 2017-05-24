package exastencils.domain.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_DomainCollection extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/domain/|LAYER_LC|/|LAYER_UC|_DomainCollection.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.domain.|LAYER_LC|

import exastencils.domain.|NEXT_LC|._
import exastencils.knowledge.|LAYER_LC|.|LAYER_UC|_KnowledgeContainer._
import exastencils.knowledge.|LAYER_LC|._

/// |LAYER_UC|_DomainCollection

object |LAYER_UC|_DomainCollection extends |LAYER_UC|_BasicKnowledgeCollection[|LAYER_UC|_Domain, |NEXT_UC|_Domain] {
  exastencils.core.Duplicate.registerConstant(this)

  |LAYER_UC|_KnowledgeContainer.register(this)

  |LAYER_UC|_PrepareDeclarations.strategies += |LAYER_UC|_PrepareDomainDeclarations
  |LAYER_UC|_ProcessDeclarations.strategies += |LAYER_UC|_ProcessDomainDeclarations

  override def name = "|LAYER_UC|_DomainCollection"
  override def progress() = objects.foreach(obj => |NEXT_UC|_DomainCollection.add(obj.progress()))
}
"""
  }
}
