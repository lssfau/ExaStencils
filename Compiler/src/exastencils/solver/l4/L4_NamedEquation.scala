package exastencils.solver.l4

import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.knowledge.l4.L4_LeveledKnowledgeObject
import exastencils.optimization.l4.L4_GeneralSimplifyWrapper
import exastencils.prettyprinting._
import exastencils.solver.ir.IR_NamedEquation

/// L4_NamedEquation

case class L4_NamedEquation(
    var name : String, var level : Int,
    var equation : L4_Equation) extends L4_LeveledKnowledgeObject[IR_NamedEquation] {

  def lhs = equation.lhs
  def rhs = equation.rhs

  override def prettyprintDecl(out : PpStream) = {
    out << "Equation " << name << '@' << level << " {\n"
    out << lhs << " == " << rhs
    out << "\n}"
  }

  override def progressImpl() = IR_NamedEquation(name, level, equation.progress)

  def asZeroEquation() : L4_Expression = {
    val zeroEq : L4_Expression = Duplicate(lhs - rhs)
    L4_GeneralSimplifyWrapper.process(zeroEq)
  }
}
