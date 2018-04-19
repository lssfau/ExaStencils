package exastencils.solver.l3

import scala.collection.mutable._

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.l3._
import exastencils.operator.l3.L3_Stencil
import exastencils.prettyprinting._

/// L3_SolverForEqEntry

case class L3_SolverForEqEntry(solName : String, eqName : String) extends L3_Node with PrettyPrintable {
  var rhsPerLevel = HashMap[Int, L3_Field]()
  var resPerLevel = HashMap[Int, L3_Field]()
  var approxPerLevel = HashMap[Int, L3_Field]()

  var restrictForResPerLevel = HashMap[Int, L3_Stencil]()
  var restrictForSolPerLevel = HashMap[Int, L3_Stencil]()
  var prolongForSolPerLevel = HashMap[Int, L3_Stencil]()

  var localEqPerLevel = HashMap[Int, L3_Equation]()

  override def prettyprint(out : PpStream) = out << solName << " in " << eqName

  def getSolField(level : Int) = L3_FieldCollection.getByIdentifier(solName, level).get
  def getEq(level : Int) = {
    if (!localEqPerLevel.contains(level))
      localEqPerLevel += (level -> Duplicate(L3_EquationCollection.getByIdentifier(eqName, level).get.equation))
    localEqPerLevel(level)
  }

  def prepEqForMG(level : Int, unknowns : ListBuffer[L3_Field]) = {
    if (Knowledge.solver_eliminateRHS) {
      // shuffle equation
      val eq = getEq(level)
      eq.splitLhsRhs(unknowns)

      // eliminate equation's rhs
      eq.rhs = 0.0
    }

    getEq(level).rhs += L3_FieldAccess(rhsPerLevel(level))
  }

  def generateUpdateRes(level : Int) : L3_Statement = {
    // Residual = RHS - LHS
    L3_Assignment(L3_FieldAccess(resPerLevel(level)),
      Duplicate(getEq(level).rhs - getEq(level).lhs))
  }

  def generateSetSolZero(level : Int) : L3_Statement = {
    // Solution = 0
    L3_Assignment(L3_FieldAccess(getSolField(level)), 0.0)
  }
}
