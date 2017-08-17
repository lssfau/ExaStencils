package exastencils.operator.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.solver.l2.L2_EquationAccess

/// L2_OperatorFromEqEntry

abstract class L2_OperatorFromEqEntry extends L2_Node {
  def targetField : L2_Access
  def mapping : ListBuffer[L2_OperatorMapping]
  def resolveEquation() : L2_Equation
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

  def resolveEquation() = equation
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

  def resolveEquation() = {
    val eqAccess = equation.asInstanceOf[L2_EquationAccess]
    val eq = Duplicate(eqAccess.target.eq)

    if (eqAccess.offset.isDefined) {
      L2_OffsetAllApplicable.offset = eqAccess.offset.get
      L2_OffsetAllApplicable.applyStandalone(eq)
    }

    eq
  }
}
