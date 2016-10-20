package exastencils.operator.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2._
import exastencils.knowledge.l2.L2_KnowledgeObjectWithLevel
import exastencils.operator.l3._
import exastencils.prettyprinting._

/// L2_Stencil

object L2_Stencil {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L2_Stencil(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var entries : ListBuffer[L2_StencilEntry]) extends L2_KnowledgeObjectWithLevel[L3_Stencil] with L2_Operator {

  override def prettyprintDecl(out : PpStream) : Unit = ???
  override def progressImpl() = L3_Stencil(name, level, entries.map(_.progress))
}

/// L2_StencilEntry

case class L2_StencilEntry(var offset : L2_Index, var coefficient : L2_Expression) extends L2_Node with L2_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << offset << " => " << coefficient
  override def progress = L3_StencilEntry(offset.progress, coefficient.progress)
}
