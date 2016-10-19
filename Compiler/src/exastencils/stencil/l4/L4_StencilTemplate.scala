package exastencils.stencil.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_Index
import exastencils.domain.l4.L4_Domain
import exastencils.knowledge.l4.L4_KnowledgeObjectWithLevel
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.stencil.ir._

///// L4_StencilTemplate
//
//object L4_StencilTemplate {
//  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
//}
//
//case class L4_StencilTemplate(
//    var name : String, // will be used to find the stencil
//    var level : Int, // the level the stencil lives on
//    var localization : String, // localization of the stencil data
//    var domain : L4_Domain, // domain the stencil lives on
//    var offsets : ListBuffer[L4_Index]) extends L4_KnowledgeObjectWithLevel {
//
//  def prettyprintDecl(out : PpStream) = {
//    out << "Stencil " << name << "@(" << level << ") {\n"
//    for (offset <- offsets)
//      out << offset << " => " << 0 << "\n"
//    out << "\n}\n"
//  }
//
//  def progress = {
//    progressed = Some(IR_StencilTemplate(name, level, domain.getProgressedObject, offsets.map(_.progress)))
//    progressed.get
//  }
//
//  var progressed : Option[IR_StencilTemplate] = None
//  override def getProgressedObject = {
//    if (progressed.isEmpty)
//      Logger.warn(s"Trying to access invalid progressed object of type ${ this.getClass.getName } with name ${ name }")
//    progressed.get
//  }
//}
