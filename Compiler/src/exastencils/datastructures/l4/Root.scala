package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.multiGrid._

case class Root(nodes : List[Node]) extends Node with ProgressableToIr {

  var domains : ListBuffer[DomainDeclarationStatement] = new ListBuffer()
  var layouts : ListBuffer[LayoutDeclarationStatement] = new ListBuffer()
  var fields : ListBuffer[FieldDeclarationStatement] = new ListBuffer()
  var externalFields : ListBuffer[ExternalFieldDeclarationStatement] = new ListBuffer()
  var stencils : ListBuffer[StencilDeclarationStatement] = new ListBuffer()
  var iterationSets : ListBuffer[IterationSetDeclarationStatement] = new ListBuffer()
  var globals : ListBuffer[GlobalDeclarationStatement] = new ListBuffer()
  var statements : ListBuffer[Statement] = new ListBuffer()

  {
    nodes.foreach(n => n match {
      case p : DomainDeclarationStatement        => domains.+=(p)
      case p : LayoutDeclarationStatement        => layouts.+=(p)
      case p : FieldDeclarationStatement         => fields.+=(p)
      case p : ExternalFieldDeclarationStatement => externalFields.+=(p)
      case p : StencilDeclarationStatement       => stencils.+=(p)
      case p : IterationSetDeclarationStatement  => iterationSets.+=(p)
      case p : GlobalDeclarationStatement        => globals.+=(p)
      case p : Statement                         => statements.+=(p)
    })

    // set domain indices -> just number consecutively
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

    globals.foreach(f => newRoot += f.progressToIr)

    var multiGrid = new MultiGrid // FIXME: think about how to manage (MG/other) functions
    for (node <- statements)
      node match {
        case function : FunctionStatement => multiGrid.functions += function.progressToIr
      }
    newRoot += multiGrid

    newRoot
  }
}
