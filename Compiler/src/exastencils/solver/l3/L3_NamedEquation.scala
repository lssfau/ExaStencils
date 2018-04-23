package exastencils.solver.l3

import exastencils.base.l3._
import exastencils.core.Duplicate
import exastencils.knowledge.l3.L3_LeveledKnowledgeObject
import exastencils.optimization.l3.L3_GeneralSimplifyWrapper
import exastencils.prettyprinting._
import exastencils.solver.l4.L4_NamedEquation

/// L3_NamedEquation

case class L3_NamedEquation(
    var name : String, var level : Int,
    var equation : L3_Equation) extends L3_LeveledKnowledgeObject[L4_NamedEquation] {

  def lhs = equation.lhs
  def rhs = equation.rhs

  override def createDuplicate() = L3_NamedEquation(name, level, Duplicate(equation))

  override def prettyprintDecl(out : PpStream) = {
    out << "Equation " << name << '@' << level << " {\n"
    out << lhs << " == " << rhs
    out << "\n}"
  }

  override def progressImpl() = L4_NamedEquation(name, level, equation.progress)

  def asZeroEquation() : L3_Expression = {
    val zeroEq : L3_Expression = Duplicate(lhs - rhs)
    L3_GeneralSimplifyWrapper.process(zeroEq)
  }
}
