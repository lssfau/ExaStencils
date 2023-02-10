//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.solver.l3

import scala.collection.mutable._

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.l3._
import exastencils.operator.l3.L3_Stencil
import exastencils.prettyprinting._

/// L3_SolverForEqEntry

// TODO: Use FieldLikeCollections instead or FieldCollection

case class L3_SolverForEqEntry(solName : String, eqName : String) extends L3_Node with PrettyPrintable {
  var rhsPerLevel = HashMap[Int, L3_Field]()
  var resPerLevel = HashMap[Int, L3_Field]()
  var errorPerLevel = HashMap[Int, L3_Field]()
  var approxPerLevel = HashMap[Int, L3_Field]()

  var restrictForResPerLevel = HashMap[Int, L3_Stencil]()
  var restrictForSolPerLevel = HashMap[Int, L3_Stencil]()
  var prolongForSolPerLevel = HashMap[Int, L3_Stencil]()

  var localEqPerLevel = HashMap[Int, L3_Equation]()

  override def prettyprint(out : PpStream) = out << solName << " in " << eqName

  var solPerLevel = HashMap[Int, L3_Field]()
  def getSolField(level : Int) = {
    if (!solPerLevel.contains(level))
      solPerLevel += (level -> L3_FieldCollection.getByIdentifier(solName, level).get)
    solPerLevel(level)
  }

  def getEq(level : Int) = {
    if (!localEqPerLevel.contains(level))
      localEqPerLevel += (level -> Duplicate(L3_EquationCollection.getByIdentifier(eqName, level).get.equation))
    localEqPerLevel(level)
  }

  def prepEqForMG(level : Int, unknowns : ListBuffer[L3_Field], unknownErrorMap : HashMap[L3_Field, L3_Field]) = {
    val eq = getEq(level)

    if (Knowledge.solver_eliminateRHS) {
      // shuffle equation
      eq.splitLhsRhs(unknowns)

      // eliminate equation's rhs
      eq.rhs = 0.0
    }

    if (Knowledge.solver_overwriteSolutionFields) {
      object L3_ReplaceAccesses extends QuietDefaultStrategy("Local replace of field accesses") {
        this += new Transformation("Search and replace", {
          case access : L3_FieldAccess if unknownErrorMap.contains(access.target) =>
            access.target = unknownErrorMap(access.target)
            access
        })
      }
      L3_ReplaceAccesses.applyStandalone(eq)
      solPerLevel(level) = errorPerLevel(level)
    }

    eq.rhs += L3_FieldAccess(rhsPerLevel(level))
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
