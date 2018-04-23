package exastencils.solver.l2

import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.knowledge.l2.L2_LeveledKnowledgeObject
import exastencils.optimization.l2.L2_GeneralSimplifyWrapper
import exastencils.prettyprinting._
import exastencils.solver.l3.L3_NamedEquation

/// L2_NamedEquation

case class L2_NamedEquation(
    var name : String, var level : Int,
    var equation : L2_Equation) extends L2_LeveledKnowledgeObject[L3_NamedEquation] {

  def lhs = equation.lhs
  def rhs = equation.rhs

  override def createDuplicate() = L2_NamedEquation(name, level, Duplicate(equation))

  override def prettyprintDecl(out : PpStream) = {
    out << "Equation " << name << '@' << level << " {\n"
    out << lhs << " == " << rhs
    out << "\n}"
  }

  override def progressImpl() = L3_NamedEquation(name, level, equation.progress)

  def asZeroEquation() : L2_Expression = {
    val zeroEq : L2_Expression = Duplicate(lhs - rhs)
    L2_GeneralSimplifyWrapper.process(zeroEq)
  }
}
