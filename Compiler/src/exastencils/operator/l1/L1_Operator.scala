package exastencils.operator.l1

import exastencils.base.l1._
import exastencils.knowledge.l1.L1_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.operator.l2._
import exastencils.prettyprinting._

/// L1_Operator

case class L1_Operator(
    var name : String, // will be used to find the operator
    var level : Int, // the level the operator lives on
    var expr : L1_Expression) extends L1_LeveledKnowledgeObject[L2_Stencil] {

  override def prettyprintDecl(out : PpStream) = out << "Operator " << name << "@" << level << " = " << expr
  override def progressImpl() = Logger.error(s"Direct progression of L1 operator $name is not supported")
}
