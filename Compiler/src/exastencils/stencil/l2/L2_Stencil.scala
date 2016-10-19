package exastencils.stencil.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2._
import exastencils.knowledge.l2._
import exastencils.prettyprinting._
import exastencils.stencil.l3._

///// L2_StencilEntry
//
//abstract class L2_StencilEntry extends L2_Node with PrettyPrintable {
//  def offset : L2_Index
//  def coefficient : Any
//
//  def progress : L3_StencilEntry
//}
//
///// L2_VarStencilEntry
//
//case class L2_VarStencilEntry(var offset : L2_Index, var coefficient : L2_Expression) extends L2_StencilEntry {
//  override def prettyprint(out : PpStream) = out << offset << " => " << coefficient
//  override def progress = L3_VarStencilEntry(offset.progress, coefficient.progress)
//}
//
///// L2_ConstStencilEntry
//
//case class L2_ConstStencilEntry(var offset : L2_Index, var coefficient : Double) extends L2_StencilEntry {
//  override def prettyprint(out : PpStream) = out << offset << " => " << coefficient
//  override def progress = L3_ConstStencilEntry(offset.progress, coefficient)
//}
//
///// L2_Stencil
//
//case class L2_Stencil(
//    var name : String, // will be used to find the stencil
//    var level : Int, // the level the stencil lives on
//    var entries : ListBuffer[L2_StencilEntry]) extends L2_KnowledgeObjectWithLevel[L3_Stencil] {
//
//  override def prettyprintDecl(out : PpStream) : Unit = ???
//  override def progressImpl() = L3_Stencil(name, level, entries.map(_.progress))
//}
