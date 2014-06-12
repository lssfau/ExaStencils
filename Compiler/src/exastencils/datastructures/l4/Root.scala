package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.multiGrid._

case class Root(nodes : List[Node]) extends Node with ProgressableToIr {

  var domains : ListBuffer[DomainDeclarationStatement] = new ListBuffer()
  var layouts : ListBuffer[LayoutDeclarationStatement] = new ListBuffer()
  var fields : ListBuffer[FieldDeclarationStatement] = new ListBuffer()
  var stencilFields : ListBuffer[StencilFieldDeclarationStatement] = new ListBuffer()
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
      case p : StencilFieldDeclarationStatement  => stencilFields.+=(p)
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

    // Domains
    DomainCollection.domains.clear
    for (domain <- domains)
      DomainCollection.domains += domain.progressToIr

    // Fields => requires Domains
    FieldCollection.fields.clear
    for (field <- fields)
      FieldCollection.fields += field.progressToIr

    // Stencils
    StencilCollection.stencils.clear
    for (stencil <- stencils)
      StencilCollection.stencils += stencil.progressToIr

    // StencilFields => requires Fields and Stencils
    StencilFieldCollection.stencilFields.clear
    for (stencilField <- stencilFields)
      StencilFieldCollection.stencilFields += stencilField.progressToIr

    // ExternalFields => requires Fields
    ExternalFieldCollection.fields.clear
    for (extField <- externalFields)
      ExternalFieldCollection.fields += extField.progressToIr

    // IterationSets
    IterationSetCollection.sets.clear
    for (iterationSet <- iterationSets)
      IterationSetCollection.sets += iterationSet.progressToIr

    globals.foreach(f => newRoot += f.progressToIr) // FIXME: this will generate multiple Global instances...

    var multiGrid = new MultiGrid // FIXME: think about how to manage (MG/other) functions
    for (node <- statements)
      node match {
        case function : FunctionStatement => multiGrid.functions += function.progressToIr
      }
    newRoot += multiGrid

    newRoot
  }
}
