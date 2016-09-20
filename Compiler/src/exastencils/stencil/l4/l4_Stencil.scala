package exastencils.stencil.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.knowledge._
import exastencils.knowledge.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_StencilEntry

case class L4_StencilEntry(var offset : L4_ExpressionIndex, var coefficient : L4_Expression) extends L4_Node with PrettyPrintable {
  // TODO: split into const/var => offset : L4_ConstIndex
  override def prettyprint(out : PpStream) = out << offset << " => " << coefficient
  def progress = StencilEntry(offset.progress, coefficient.progress)
}

/// L4_Stencil

case class L4_Stencil(var identifier : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var entries : ListBuffer[L4_StencilEntry]) extends L4_KnowledgeObjectWithIdentAndLevel {

  override def prettyprintDecl(out : PpStream) = {
    out << "Stencil " << identifier << "@(" << level << ") {\n"
    out <<< (entries, "\n")
    out << "\n}\n"
  }

  def progress = {
    progressed = Some(Stencil(identifier, level, entries.map(_.progress)))
    progressed.get
  }

  var progressed : Option[Stencil] = None
  override def getProgressedObject = {
    if (progressed.isEmpty)
      Logger.warn(s"Trying to access invalid progressed object of type ${ this.getClass.getName } with name ${ identifier }")
    progressed.get
  }
}