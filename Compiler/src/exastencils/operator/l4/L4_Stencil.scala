package exastencils.operator.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.knowledge.l4.L4_KnowledgeObjectWithLevel
import exastencils.operator.ir._
import exastencils.prettyprinting._

/// L4_Stencil

object L4_Stencil {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L4_Stencil(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var entries : ListBuffer[L4_StencilEntry]) extends L4_KnowledgeObjectWithLevel[IR_Stencil] with L4_Operator {

  override def prettyprintDecl(out : PpStream) = {
    out << "Stencil " << name << "@(" << level << ") {\n"
    out <<< (entries, "\n")
    out << "\n}"
  }

  override def progressImpl() = IR_Stencil(name, level, entries.map(_.progress))
}

/// L4_StencilEntry

case class L4_StencilEntry(var offset : L4_Index, var coefficient : L4_Expression) extends L4_Node with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << offset << " => " << coefficient
  def progress = IR_StencilEntry(offset.progress.toExpressionIndex, coefficient.progress)
}
