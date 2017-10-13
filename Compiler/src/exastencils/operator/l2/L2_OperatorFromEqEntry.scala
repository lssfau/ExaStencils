package exastencils.operator.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.prettyprinting._
import exastencils.solver.l2.L2_EquationAccess

/// L2_OperatorFromEqEntry

abstract class L2_OperatorFromEqEntry extends L2_Node with PrettyPrintable {
  def targetField : L2_Access
  def mapping : ListBuffer[L2_OperatorMapping]
  def resolveEquation() : L2_Equation
  def updateEquation(eq : L2_Equation) : Unit
}

/// L2_OperatorFromInlineEq

object L2_OperatorFromInlineEq {
  def apply(targetField : L2_Access, equation : L2_Equation, mapping : List[L2_OperatorMapping]) =
    new L2_OperatorFromInlineEq(targetField, equation, mapping.to[ListBuffer])
}

case class L2_OperatorFromInlineEq(
    var targetField : L2_Access,
    var equation : L2_Equation,
    var mapping : ListBuffer[L2_OperatorMapping]) extends L2_OperatorFromEqEntry {

  override def prettyprint(out : PpStream) = {
    out << "equation for " << targetField << " {\n"
    out << equation << "\n} store in {\n"
    out <<< (mapping, "\n") << "\n}"
  }

  override def resolveEquation() = equation
  override def updateEquation(eq : L2_Equation) = { equation = eq }
}

/// L2_OperatorFromNamedEq

object L2_OperatorFromNamedEq {
  def apply(targetField : L2_Access, equation : L2_Access, mapping : List[L2_OperatorMapping]) =
    new L2_OperatorFromNamedEq(targetField, equation, mapping.to[ListBuffer])
}

case class L2_OperatorFromNamedEq(
    var targetField : L2_Access,
    var equation : L2_Access,
    var mapping : ListBuffer[L2_OperatorMapping]) extends L2_OperatorFromEqEntry {

  override def prettyprint(out : PpStream) = {
    out << "equation for " << targetField << " is " << equation << " store in {\n"
    out <<< (mapping, "\n") << "\n}"
  }

  def resolveEquation() = {
    val eqAccess = equation.asInstanceOf[L2_EquationAccess]
    val eq = Duplicate(eqAccess.target.equation)

    if (eqAccess.offset.isDefined) {
      L2_OffsetAllApplicable.offset = eqAccess.offset.get
      L2_OffsetAllApplicable.applyStandalone(eq)
    }

    eq
  }

  def updateEquation(eq : L2_Equation) = {
    val eqAccess = equation.asInstanceOf[L2_EquationAccess]
    eqAccess.target.equation = eq
  }
}
