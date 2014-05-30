package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.multiGrid._

case class Root(
    var domains : List[DomainDeclarationStatement],
    var layouts : List[LayoutDeclarationStatement],
    var fields : List[FieldDeclarationStatement],
    var externalFields : List[ExternalFieldDeclarationStatement],
    var stencils : List[StencilDeclarationStatement],
    var iterationSets : List[IterationSetDeclarationStatement],
    var statements : List[Statement]) extends Node with ProgressableToIr {

  // set domain indices -> just number consecutively
  {
    var i = 0
    for (d <- domains) {
      d.index = i
      i += 1
    }
  }

  def getDomainByIdentifier(identifier : String) : Option[DomainDeclarationStatement] = {
    domains.find(d => d.name == identifier)
  }

  def getLayoutByIdentifier(identifier : String) : Option[LayoutDeclarationStatement] = {
    layouts.find(l => l.name == identifier)
  }

  def getFieldByIdentifier(identifier : String, level : Int) : Option[FieldDeclarationStatement] = {
    fields.find(f => f.name == identifier && f.level.getOrElse(-1) == level)
  }

  def progressToIr : Node = {
    var newRoot = new ir.Root

    var domainCollection = new DomainCollection
    for (domain <- domains)
      domainCollection.domains += domain.progressToIr
    newRoot += domainCollection

    var fieldCollection = new FieldCollection
    for (field <- fields)
      fieldCollection.fields += field.progressToIr
    newRoot += fieldCollection

    var extFieldCollection = new ExternalFieldCollection
    for (extField <- externalFields)
      extFieldCollection.fields += extField.progressToIr
    newRoot += extFieldCollection

    var stencilCollection = new StencilCollection
    for (stencil <- stencils)
      stencilCollection.stencils += stencil.progressToIr
    newRoot += stencilCollection

    var iterationSetCollection = new IterationSetCollection
    for (iterationSet <- iterationSets)
      iterationSetCollection.sets += iterationSet.progressToIr
    newRoot += iterationSetCollection

    var multiGrid = new MultiGrid // FIXME: think about how to manage (MG/other) functions
    for (node <- statements)
      node match {
        case function : FunctionStatement => multiGrid.functions += function.progressToIr
      }
    newRoot += multiGrid

    newRoot
  }
}
