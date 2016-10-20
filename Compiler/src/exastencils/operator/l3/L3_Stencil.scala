package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.knowledge.l3.L3_KnowledgeObjectWithLevel
import exastencils.operator.l4._
import exastencils.prettyprinting._

/// L3_Stencil

object L3_Stencil {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L3_Stencil(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var entries : ListBuffer[L3_StencilEntry]) extends L3_KnowledgeObjectWithLevel[L4_Stencil] with L3_Operator {

  override def prettyprintDecl(out : PpStream) : Unit = ???
  override def progressImpl() = L4_Stencil(name, level, entries.map(_.progress))
}

/// L3_StencilEntry

case class L3_StencilEntry(var offset : L3_Index, var coefficient : L3_Expression) extends L3_Node with L3_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = { out << offset << " => " << coefficient }
  override def progress = L4_StencilEntry(offset.progress, coefficient.progress)
}
