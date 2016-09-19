package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.domain.l4.L4_DomainDeclaration
import exastencils.globals._
import exastencils.knowledge._
import exastencils.multiGrid._
import exastencils.prettyprinting._

case class Root()(nodes : List[Node]) extends Node with L4_Progressable with PrettyPrintable {

  var domains : ListBuffer[L4_DomainDeclaration] = new ListBuffer()
  var fieldLayouts : ListBuffer[LayoutDeclarationStatement] = new ListBuffer()
  var fields : ListBuffer[FieldDeclarationStatement] = new ListBuffer()
  var stencilFields : ListBuffer[StencilFieldDeclarationStatement] = new ListBuffer()
  var externalFields : ListBuffer[ExternalFieldDeclarationStatement] = new ListBuffer()
  var stencils : ListBuffer[StencilDeclarationStatement] = new ListBuffer()
  var globals : ListBuffer[GlobalDeclarationStatement] = new ListBuffer()
  var functionTemplates : ListBuffer[FunctionTemplateStatement] = new ListBuffer()
  var functions : ListBuffer[FunctionStatement] = new ListBuffer()
  var statements : ListBuffer[L4_Statement] = new ListBuffer()

  nodes.foreach {
    case p : L4_DomainDeclaration              => domains.+=(p)
    case p : LayoutDeclarationStatement        => fieldLayouts.+=(p)
    case p : FieldDeclarationStatement         => fields.+=(p)
    case p : StencilFieldDeclarationStatement  => stencilFields.+=(p)
    case p : ExternalFieldDeclarationStatement => externalFields.+=(p)
    case p : StencilDeclarationStatement       => stencils.+=(p)
    case p : GlobalDeclarationStatement        => globals.+=(p)
    case p : FunctionTemplateStatement         => functionTemplates.+=(p)
    case p : FunctionStatement                 => functions.+=(p)
    case p : L4_Statement                      => statements.+=(p)
    case r : Root                              =>
      domains.++=(r.domains)
      fieldLayouts.++=(r.fieldLayouts)
      fields.++=(r.fields)
      stencilFields.++=(r.stencilFields)
      externalFields.++=(r.externalFields)
      stencils.++=(r.stencils)
      globals.++=(r.globals)
      functionTemplates.++=(r.functionTemplates)
      functions.++=(r.functions)
      statements.++=(r.statements)
  }

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

  override def prettyprint(out : PpStream) : Unit = {
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
    if (!functionTemplates.isEmpty)
      out <<< (functionTemplates, "\n") << '\n'
    if (!functions.isEmpty)
      out <<< (functions, "\n") << '\n'
    if (!statements.isEmpty)
      out <<< (statements, "\n") << '\n'
  }

  override def progress : IR_Root = {
    var newRoot = IR_Root()

    // Domains
    DomainCollection.domains.clear
    for (domain <- domains)
      DomainCollection.domains += domain.progress

    // FieldLayouts
    FieldLayoutCollection.fieldLayouts.clear
    if (!Knowledge.ir_genSepLayoutsPerField) {
      for (fieldLayout <- fieldLayouts)
        FieldLayoutCollection.fieldLayouts += fieldLayout.progress("")
    }

    // Fields => requires Domains and FieldLayouts
    FieldCollection.fields.clear
    for (field <- fields)
      FieldCollection.fields += field.progress

    // Stencils
    StencilCollection.stencils.clear
    for (stencil <- stencils)
      StencilCollection.stencils += stencil.progress

    // StencilFields => requires Fields and Stencils
    StencilFieldCollection.stencilFields.clear
    for (stencilField <- stencilFields)
      StencilFieldCollection.stencilFields += stencilField.progress

    // ExternalFields => requires Fields
    ExternalFieldCollection.fields.clear
    for (extField <- externalFields)
      ExternalFieldCollection.fields += extField.progress

    // Globals
    var progGlobals = new Globals(new ListBuffer)
    globals.foreach(f => progGlobals.variables ++= f.progress)
    newRoot += progGlobals

    var multiGrid = new MultiGridFunctions // FIXME: think about how to manage (MG/other) functions
    functions.foreach(multiGrid.functions += _.progress)
    newRoot += multiGrid

    newRoot
  }
}
