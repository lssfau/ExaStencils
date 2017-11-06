package exastencils.solver.l1

import exastencils.base.l1._
import exastencils.core.Duplicate
import exastencils.knowledge.l1.L1_LeveledKnowledgeObject
import exastencils.optimization.l1.L1_GeneralSimplifyWrapper
import exastencils.prettyprinting._
import exastencils.solver.l2.L2_NamedEquation

/// L1_NamedEquation

case class L1_NamedEquation(
    var name : String, var level : Int,
    var equation : L1_Equation) extends L1_LeveledKnowledgeObject[L2_NamedEquation] {

  def lhs = equation.lhs
  def rhs = equation.rhs

  override def prettyprintDecl(out : PpStream) = {
    out << "Equation " << name << '@' << level << " {\n"
    out << lhs << " == " << rhs
    out << "\n}"
  }

  override def progressImpl() = L2_NamedEquation(name, level, equation.progress)

  def asZeroEquation() : L1_Expression = {
    val zeroEq : L1_Expression = Duplicate(lhs - rhs)
    L1_GeneralSimplifyWrapper.process(zeroEq)
  }
}
