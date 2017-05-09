package exastencils.operator.l2

import scala.collection.mutable.ListBuffer

import exastencils.knowledge.l2.L2_KnowledgeObjectWithLevel
import exastencils.operator.l3._
import exastencils.prettyprinting._

/// L2_Stencil

case class L2_Stencil(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var entries : ListBuffer[L2_StencilEntry]) extends L2_KnowledgeObjectWithLevel[L3_Stencil] with L2_Operator {

  override def prettyprintDecl(out : PpStream) : Unit = ???
  override def progressImpl() = L3_Stencil(name, level, entries.map(_.progress))
}
