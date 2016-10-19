package exastencils.stencil.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.base.l4.L4_RealConstant
import exastencils.knowledge.l3.L3_KnowledgeObjectWithLevel
import exastencils.prettyprinting._
import exastencils.stencil.l4._

///// L3_StencilEntry
//
//abstract class L3_StencilEntry extends L3_Node with PrettyPrintable {
//  def offset : L3_Index
//  def coefficient : Any
//
//  def progress : L4_StencilEntry
//}
//
///// L3_VarStencilEntry
//
//case class L3_VarStencilEntry(var offset : L3_Index, var coefficient : L3_Expression) extends L3_StencilEntry {
//  override def prettyprint(out : PpStream) = { out << offset << " => " << coefficient }
//  override def progress = { L4_StencilEntry(offset.progress, coefficient.progress) }
//}
//
///// L3_ConstStencilEntry
//
//case class L3_ConstStencilEntry(var offset : L3_Index, var coefficient : Double) extends L3_StencilEntry {
//  override def prettyprint(out : PpStream) = { out << offset << " => " << coefficient }
//  override def progress = { L4_StencilEntry(offset.progress, L4_RealConstant(coefficient)) }
//}
//
///// L3_Stencil
//
//case class L3_Stencil(
//    var name : String, // will be used to find the stencil
//    var level : Int, // the level the stencil lives on
//    var entries : ListBuffer[L3_StencilEntry]) extends L3_KnowledgeObjectWithLevel {
//
//  def progress = L4_Stencil(name, level, entries.map(_.progress))
//}
