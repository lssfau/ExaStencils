package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.multiGrid._

case class Root(
    var domains : List[DomainDeclarationStatement],
    var layouts : List[LayoutDeclarationStatement],
    var fields : List[FieldDeclarationStatement],
    var stencilFields : List[StencilFieldDeclarationStatement],
    var externalFields : List[ExternalFieldDeclarationStatement],
    var stencils : List[StencilDeclarationStatement],
    var iterationSets : List[IterationSetDeclarationStatement],
    var globals : GlobalDeclarationStatement,
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

    DomainCollection.domains.clear
    for (domain <- domains)
      DomainCollection.domains += domain.progressToIr

    FieldCollection.fields.clear
    for (field <- fields)
      FieldCollection.fields += field.progressToIr

    StencilCollection.stencils.clear
    for (stencil <- stencils)
      StencilCollection.stencils += stencil.progressToIr

    StencilFieldCollection.stencilFields.clear
    for (stencilField <- stencilFields)
      StencilFieldCollection.stencilFields += stencilField.progressToIr

    ExternalFieldCollection.fields.clear
    for (extField <- externalFields)
      ExternalFieldCollection.fields += extField.progressToIr

    IterationSetCollection.sets.clear
    for (iterationSet <- iterationSets)
      IterationSetCollection.sets += iterationSet.progressToIr

    newRoot += globals.progressToIr

    var multiGrid = new MultiGrid // FIXME: think about how to manage (MG/other) functions
    for (node <- statements)
      node match {
        case function : FunctionStatement => multiGrid.functions += function.progressToIr
      }
    newRoot += multiGrid

    newRoot
  }
}
