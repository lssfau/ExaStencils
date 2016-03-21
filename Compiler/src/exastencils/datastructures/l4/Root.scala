package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.globals._
import exastencils.knowledge._
import exastencils.multiGrid._
import exastencils.prettyprinting._

case class Root() extends Node with ProgressableToIr with PrettyPrintable {

  var domains : ListBuffer[DomainDeclarationStatement] = new ListBuffer()
  var fieldLayouts : ListBuffer[LayoutDeclarationStatement] = new ListBuffer()
  var fields : ListBuffer[FieldDeclarationStatement] = new ListBuffer()
  var stencilFields : ListBuffer[StencilFieldDeclarationStatement] = new ListBuffer()
  var externalFields : ListBuffer[ExternalFieldDeclarationStatement] = new ListBuffer()
  var stencils : ListBuffer[StencilDeclarationStatement] = new ListBuffer()
  var globals : ListBuffer[GlobalDeclarationStatement] = new ListBuffer()
  var functionTemplates : ListBuffer[FunctionTemplateStatement] = new ListBuffer()
  var statements : ListBuffer[Statement] = new ListBuffer()

  def addNodes(nodes : List[Node]) = {
    nodes.foreach(n => n match {
      case p : DomainDeclarationStatement        => domains.+=(p)
      case p : LayoutDeclarationStatement        => fieldLayouts.+=(p)
      case p : FieldDeclarationStatement         => fields.+=(p)
      case p : StencilFieldDeclarationStatement  => stencilFields.+=(p)
      case p : ExternalFieldDeclarationStatement => externalFields.+=(p)
      case p : StencilDeclarationStatement       => stencils.+=(p)
      case p : GlobalDeclarationStatement        => globals.+=(p)
      case p : FunctionTemplateStatement         => functionTemplates.+=(p)
      case p : Statement                         => statements.+=(p)
    })

    // set domain indices -> just number consecutively
    var i = domains.size
    for (d <- domains) {
      d.index = i
      i += 1
    }
    // set field indices -> just number consecutively
    i = fields.size
    for (f <- fields) {
      f.index = i
      i += 1
    }
  }

  override def prettyprint(out : PpStream): Unit = {
    if (!domains.isEmpty)
      out <<< domains << '\n'
    if (!fieldLayouts.isEmpty)
      out <<< fieldLayouts << '\n'
    if (!fields.isEmpty)
      out <<< fields << '\n'
    if (!stencilFields.isEmpty)
      out <<< stencilFields << '\n'
    if (!externalFields.isEmpty)
      out <<< externalFields << '\n'
    if (!stencils.isEmpty)
      out <<< stencils << '\n'
    if (!globals.isEmpty)
      out <<< globals << '\n'
    if (!statements.isEmpty)
      out <<< (statements, "\n") << '\n'
  }

  override def progressToIr : Node = {
    var newRoot = new ir.Root

    // Domains
    DomainCollection.domains.clear
    for (domain <- domains)
      DomainCollection.domains += domain.progressToIr

    // FieldLayouts
    FieldLayoutCollection.fieldLayouts.clear
    if (!Knowledge.ir_genSepLayoutsPerField) {
      for (fieldLayout <- fieldLayouts)
        FieldLayoutCollection.fieldLayouts += fieldLayout.progressToIr("")
    }

    // Fields => requires Domains and FieldLayouts
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

    // Globals
    var progGlobals = new Globals(new ListBuffer)
    globals.foreach(f => progGlobals.variables ++= f.progressToIr)
    newRoot += progGlobals

    var multiGrid = new MultiGridFunctions // FIXME: think about how to manage (MG/other) functions
    for (node <- statements)
      node match {
        case function : FunctionStatement => multiGrid.functions += function.progressToIr
      }
    newRoot += multiGrid
    newRoot
  }
}
