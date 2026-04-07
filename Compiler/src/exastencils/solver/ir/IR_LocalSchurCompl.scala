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

package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.boundary.ir.IR_IsValidComputationPoint
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.field.ir.IR_SlotAccess
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.logger.Logger

/// IR_LocalSchurCompl

object IR_LocalSchurCompl {
  def vecComponentAccess(vec : IR_VariableAccess, i0 : Int) = IR_HighDimAccess(vec, IR_ConstIndex(i0))
  def matComponentAccess(mat : IR_VariableAccess, i0 : Int, i1 : Int) = IR_HighDimAccess(mat, IR_ConstIndex(i0, i1))

  def apply(AVals : ListBuffer[ListBuffer[IR_Expression]], fVals : ListBuffer[IR_Expression], unknowns : ListBuffer[IR_FieldLikeAccess],
      jacobiType : Boolean, relax : Option[IR_Expression], omitConditions : Boolean) = {

    // TODO: currently assumes special case of 2D/3D velocity-pressure coupling

    Knowledge.dimensionality match {
      case 2 => invert2D(AVals, fVals, unknowns, jacobiType, relax, omitConditions)
      case 3 => invert3D(AVals, fVals, unknowns, jacobiType, relax, omitConditions)
    }
  }

  def suitable(AVals : ListBuffer[ListBuffer[IR_Addition]]) : Boolean = {
    // TODO: currently assumes special case of ordered velocity-pressure coupling

    val numDims = Knowledge.dimensionality // FIXME

    if (numDims < 2) // ignore small systems
      return false

    val numBlocks = numDims
    val blockSize = 2
    val systemSize = numBlocks * blockSize + 1

    // check matrix dimensions
    if (systemSize != AVals.size)
      return false

    var onlyZeros = true
    for (i <- 0 until systemSize; j <- 0 until systemSize; if (
      i != systemSize - 1 // ignore last row
        && j != systemSize - 1 // ignore last column
        && i / 2 != j / 2 // ignore local blocks
      )) AVals(i)(j).summands.foreach {
      case IR_RealConstant(0.0)  =>
      case IR_IntegerConstant(0) =>
      case other                 =>
        Logger.warn(s"Schur complement not possible due to entry $i, $j: $other")
        onlyZeros = false
    }

    onlyZeros
  }

  def invert2D(AVals : ListBuffer[ListBuffer[IR_Expression]], fVals : ListBuffer[IR_Expression], unknowns : ListBuffer[IR_FieldLikeAccess],
      jacobiType : Boolean, relax : Option[IR_Expression], omitConditions : Boolean) : ListBuffer[IR_Statement] = {

    val stmts = ListBuffer[IR_Statement]()

    def U1 = IR_VariableAccess("_local_U1", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def U2 = IR_VariableAccess("_local_U2", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def V = IR_VariableAccess("_local_V", IR_MatrixDatatype(IR_RealDatatype, 1, 1))
    def F1 = IR_VariableAccess("_local_F1", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def F2 = IR_VariableAccess("_local_F2", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def G = IR_VariableAccess("_local_G", IR_MatrixDatatype(IR_RealDatatype, 1, 1))
    def A11 = IR_VariableAccess("_local_A11", IR_MatrixDatatype(IR_RealDatatype, 2, 2))
    def A22 = IR_VariableAccess("_local_A22", IR_MatrixDatatype(IR_RealDatatype, 2, 2))
    def B1 = IR_VariableAccess("_local_B1", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def B2 = IR_VariableAccess("_local_B2", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def C1 = IR_VariableAccess("_local_C1", IR_MatrixDatatype(IR_RealDatatype, 1, 2))
    def C2 = IR_VariableAccess("_local_C2", IR_MatrixDatatype(IR_RealDatatype, 1, 2))
    def D = IR_VariableAccess("_local_D", IR_MatrixDatatype(IR_RealDatatype, 1, 1))

    // declare local variables -> to be merged later
    for (local <- List(U1, U2, V, F1, F2, G, A11, A22, B1, B2, C1, C2, D))
      stmts += IR_VariableDeclaration(local)

    def f(i : Int) = i match {
      case 0 | 1 => F1
      case 2 | 3 => F2
      case 4     => G
    }

    def u(i : Int) = i match {
      case 0 | 1 => U1
      case 2 | 3 => U2
      case 4     => V
    }

    // construct rhs and matrix
    for (i <- unknowns.indices) {
      var innerStmts = ListBuffer[IR_Statement]()
      var boundaryStmts = ListBuffer[IR_Statement]()

      // rhs for inner
      i match {
        case 0 | 1 => innerStmts += IR_Assignment(vecComponentAccess(f(i), i - 0), fVals(i))
        case 2 | 3 => innerStmts += IR_Assignment(vecComponentAccess(f(i), i - 2), fVals(i))
        case 4     => innerStmts += IR_Assignment(vecComponentAccess(f(i), i - 4), fVals(i))
      }

      // sub-matrices for inner
      i match {
        case 0 | 1 =>
          innerStmts += IR_Assignment(matComponentAccess(A11, i - 0, 0), AVals(i)(0))
          innerStmts += IR_Assignment(matComponentAccess(A11, i - 0, 1), AVals(i)(1))
          innerStmts += IR_Assignment(matComponentAccess(B1, i - 0, 0), AVals(i)(4))
        case 2 | 3 =>
          innerStmts += IR_Assignment(matComponentAccess(A22, i - 2, 0), AVals(i)(2))
          innerStmts += IR_Assignment(matComponentAccess(A22, i - 2, 1), AVals(i)(3))
          innerStmts += IR_Assignment(matComponentAccess(B2, i - 2, 0), AVals(i)(4))
        case 4     =>
          innerStmts += IR_Assignment(matComponentAccess(C1, 0, 0 - 0), AVals(i)(0))
          innerStmts += IR_Assignment(matComponentAccess(C1, 0, 1 - 0), AVals(i)(1))
          innerStmts += IR_Assignment(matComponentAccess(C2, 0, 2 - 2), AVals(i)(2))
          innerStmts += IR_Assignment(matComponentAccess(C2, 0, 3 - 2), AVals(i)(3))
          innerStmts += IR_Assignment(matComponentAccess(D, i - 4, 0), AVals(i)(4))
      }

      // rhs for boundary
      boundaryStmts += IR_Assignment(vecComponentAccess(f(i), i % 2), Duplicate(unknowns(i)))

      // sub-matrices for inner
      i match {
        case 0 | 1 =>
          boundaryStmts += IR_Assignment(matComponentAccess(A11, i - 0, 0), if (0 == i - 0) 1 else 0)
          boundaryStmts += IR_Assignment(matComponentAccess(A11, i - 0, 1), if (1 == i - 0) 1 else 0)
          boundaryStmts += IR_Assignment(matComponentAccess(B1, i - 0, 0), 0.0)
        case 2 | 3 =>
          boundaryStmts += IR_Assignment(matComponentAccess(A22, i - 2, 0), if (0 == i - 2) 1 else 0)
          boundaryStmts += IR_Assignment(matComponentAccess(A22, i - 2, 1), if (1 == i - 2) 1 else 0)
          boundaryStmts += IR_Assignment(matComponentAccess(B2, i - 2, 0), 0.0)
        case 4     =>
          boundaryStmts += IR_Assignment(matComponentAccess(C1, 0, 0 - 0), 0.0)
          boundaryStmts += IR_Assignment(matComponentAccess(C1, 0, 1 - 0), 0.0)
          boundaryStmts += IR_Assignment(matComponentAccess(C2, 0, 2 - 2), 0.0)
          boundaryStmts += IR_Assignment(matComponentAccess(C2, 0, 3 - 2), 0.0)
          boundaryStmts += IR_Assignment(matComponentAccess(D, i - 4, 0), 1.0)
      }

      // implement check if current unknown is on/ beyond boundary - if required
      if (omitConditions) {
        stmts ++= innerStmts
      } else {
        stmts += IR_IfCondition(
          IR_IsValidComputationPoint(unknowns(i).field, Duplicate(unknowns(i).index)),
          innerStmts,
          boundaryStmts)
      }
    }

    /// solve local system

    // pre-compute inverse's of local sub-matrices
    def A11Inv = IR_VariableAccess("_local_A11Inv", IR_MatrixDatatype(IR_RealDatatype, 2, 2))

    def A22Inv = IR_VariableAccess("_local_A22Inv", IR_MatrixDatatype(IR_RealDatatype, 2, 2))

    // TODO: add return types
    stmts += IR_VariableDeclaration(A11Inv, IR_FunctionCall("inverse", A11))
    stmts += IR_VariableDeclaration(A22Inv, IR_FunctionCall("inverse", A22))

    def S = IR_VariableAccess("_local_S", IR_MatrixDatatype(IR_RealDatatype, 1, 1))

    def GTilde = IR_VariableAccess("_local_GTilde", IR_MatrixDatatype(IR_RealDatatype, 1, 1))

    stmts += IR_VariableDeclaration(S, D - (C1 * A11Inv * B1 + C2 * A22Inv * B2))
    stmts += IR_VariableDeclaration(GTilde, G - C1 * A11Inv * F1 - C2 * A22Inv * F2)

    stmts += IR_Assignment(V, IR_FunctionCall("inverse", S) * GTilde)
    stmts += IR_Assignment(U1, A11Inv * (F1 - B1 * V))
    stmts += IR_Assignment(U2, A22Inv * (F2 - B2 * V))

    // write back results
    for (i <- unknowns.indices) {
      val dest = Duplicate(unknowns(i))
      if (jacobiType) dest.slot.asInstanceOf[IR_SlotAccess].offset += 1

      if (omitConditions) {
        if (relax.isEmpty)
          stmts += IR_Assignment(dest, vecComponentAccess(u(i), i % 2))
        else
          stmts += IR_Assignment(dest, Duplicate(unknowns(i)) * (1.0 - relax.get) + relax.get * vecComponentAccess(u(i), i % 2))
      } else {
        stmts += IR_IfCondition( // don't write back result on boundaries
          IR_IsValidComputationPoint(unknowns(i).field, Duplicate(unknowns(i).index)),
          if (relax.isEmpty)
            IR_Assignment(dest, vecComponentAccess(u(i), i % 2))
          else
            IR_Assignment(dest, Duplicate(unknowns(i)) * (1.0 - relax.get) + relax.get * vecComponentAccess(u(i), i % 2))
        )
      }
    }

    stmts
  }

  def invert3D(AVals : ListBuffer[ListBuffer[IR_Expression]], fVals : ListBuffer[IR_Expression], unknowns : ListBuffer[IR_FieldLikeAccess],
      jacobiType : Boolean, relax : Option[IR_Expression], omitConditions : Boolean) : ListBuffer[IR_Statement] = {

    val stmts = ListBuffer[IR_Statement]()

    def U1 = IR_VariableAccess("_local_U1", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def U2 = IR_VariableAccess("_local_U2", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def U3 = IR_VariableAccess("_local_U3", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def V = IR_VariableAccess("_local_V", IR_MatrixDatatype(IR_RealDatatype, 1, 1))
    def F1 = IR_VariableAccess("_local_F1", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def F2 = IR_VariableAccess("_local_F2", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def F3 = IR_VariableAccess("_local_F3", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def G = IR_VariableAccess("_local_G", IR_MatrixDatatype(IR_RealDatatype, 1, 1))
    def A11 = IR_VariableAccess("_local_A11", IR_MatrixDatatype(IR_RealDatatype, 2, 2))
    def A22 = IR_VariableAccess("_local_A22", IR_MatrixDatatype(IR_RealDatatype, 2, 2))
    def A33 = IR_VariableAccess("_local_A33", IR_MatrixDatatype(IR_RealDatatype, 2, 2))
    def B1 = IR_VariableAccess("_local_B1", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def B2 = IR_VariableAccess("_local_B2", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def B3 = IR_VariableAccess("_local_B3", IR_MatrixDatatype(IR_RealDatatype, 2, 1))
    def C1 = IR_VariableAccess("_local_C1", IR_MatrixDatatype(IR_RealDatatype, 1, 2))
    def C2 = IR_VariableAccess("_local_C2", IR_MatrixDatatype(IR_RealDatatype, 1, 2))
    def C3 = IR_VariableAccess("_local_C3", IR_MatrixDatatype(IR_RealDatatype, 1, 2))
    def D = IR_VariableAccess("_local_D", IR_MatrixDatatype(IR_RealDatatype, 1, 1))

    // declare local variables -> to be merged later
    for (local <- List(U1, U2, U3, V, F1, F2, F3, G, A11, A22, A33, B1, B2, B3, C1, C2, C3, D))
      stmts += IR_VariableDeclaration(local)

    def f(i : Int) = i match {
      case 0 | 1 => F1
      case 2 | 3 => F2
      case 4 | 5 => F3
      case 6     => G
    }

    def u(i : Int) = i match {
      case 0 | 1 => U1
      case 2 | 3 => U2
      case 4 | 5 => U3
      case 6     => V
    }

    // construct rhs and matrix
    for (i <- unknowns.indices) {
      var innerStmts = ListBuffer[IR_Statement]()
      var boundaryStmts = ListBuffer[IR_Statement]()

      // rhs for inner
      i match {
        case 0 | 1 => innerStmts += IR_Assignment(vecComponentAccess(f(i), i - 0), fVals(i))
        case 2 | 3 => innerStmts += IR_Assignment(vecComponentAccess(f(i), i - 2), fVals(i))
        case 4 | 5 => innerStmts += IR_Assignment(vecComponentAccess(f(i), i - 4), fVals(i))
        case 6     => innerStmts += IR_Assignment(vecComponentAccess(f(i), i - 6), fVals(i))
      }

      // sub-matrices for inner
      i match {
        case 0 | 1 =>
          innerStmts += IR_Assignment(matComponentAccess(A11, i - 0, 0), AVals(i)(0))
          innerStmts += IR_Assignment(matComponentAccess(A11, i - 0, 1), AVals(i)(1))
          innerStmts += IR_Assignment(matComponentAccess(B1, i - 0, 0), AVals(i)(6))
        case 2 | 3 =>
          innerStmts += IR_Assignment(matComponentAccess(A22, i - 2, 0), AVals(i)(2))
          innerStmts += IR_Assignment(matComponentAccess(A22, i - 2, 1), AVals(i)(3))
          innerStmts += IR_Assignment(matComponentAccess(B2, i - 2, 0), AVals(i)(6))
        case 4 | 5 =>
          innerStmts += IR_Assignment(matComponentAccess(A33, i - 4, 0), AVals(i)(4))
          innerStmts += IR_Assignment(matComponentAccess(A33, i - 4, 1), AVals(i)(5))
          innerStmts += IR_Assignment(matComponentAccess(B3, i - 4, 0), AVals(i)(6))
        case 6     =>
          innerStmts += IR_Assignment(matComponentAccess(C1, 0, 0 - 0), AVals(i)(0))
          innerStmts += IR_Assignment(matComponentAccess(C1, 0, 1 - 0), AVals(i)(1))
          innerStmts += IR_Assignment(matComponentAccess(C2, 0, 2 - 2), AVals(i)(2))
          innerStmts += IR_Assignment(matComponentAccess(C2, 0, 3 - 2), AVals(i)(3))
          innerStmts += IR_Assignment(matComponentAccess(C3, 0, 4 - 4), AVals(i)(4))
          innerStmts += IR_Assignment(matComponentAccess(C3, 0, 5 - 4), AVals(i)(5))
          innerStmts += IR_Assignment(matComponentAccess(D, i - 6, 0), AVals(i)(6))
      }

      // rhs for boundary
      boundaryStmts += IR_Assignment(vecComponentAccess(f(i), i % 2), Duplicate(unknowns(i)))

      // sub-matrices for inner
      i match {
        case 0 | 1 =>
          boundaryStmts += IR_Assignment(matComponentAccess(A11, i - 0, 0), if (0 == i - 0) 1 else 0)
          boundaryStmts += IR_Assignment(matComponentAccess(A11, i - 0, 1), if (1 == i - 0) 1 else 0)
          boundaryStmts += IR_Assignment(matComponentAccess(B1, i - 0, 0), 0.0)
        case 2 | 3 =>
          boundaryStmts += IR_Assignment(matComponentAccess(A22, i - 2, 0), if (0 == i - 2) 1 else 0)
          boundaryStmts += IR_Assignment(matComponentAccess(A22, i - 2, 1), if (1 == i - 2) 1 else 0)
          boundaryStmts += IR_Assignment(matComponentAccess(B2, i - 2, 0), 0.0)
        case 4 | 5 =>
          boundaryStmts += IR_Assignment(matComponentAccess(A33, i - 4, 0), if (0 == i - 4) 1 else 0)
          boundaryStmts += IR_Assignment(matComponentAccess(A33, i - 4, 1), if (1 == i - 4) 1 else 0)
          boundaryStmts += IR_Assignment(matComponentAccess(B3, i - 4, 0), 0.0)
        case 6     =>
          boundaryStmts += IR_Assignment(matComponentAccess(C1, 0, 0 - 0), 0.0)
          boundaryStmts += IR_Assignment(matComponentAccess(C1, 0, 1 - 0), 0.0)
          boundaryStmts += IR_Assignment(matComponentAccess(C2, 0, 2 - 2), 0.0)
          boundaryStmts += IR_Assignment(matComponentAccess(C2, 0, 3 - 2), 0.0)
          boundaryStmts += IR_Assignment(matComponentAccess(C3, 0, 4 - 4), 0.0)
          boundaryStmts += IR_Assignment(matComponentAccess(C3, 0, 5 - 4), 0.0)
          boundaryStmts += IR_Assignment(matComponentAccess(D, i - 6, 0), 1.0)
      }

      // implement check if current unknown is on/ beyond boundary - if required
      if (omitConditions) {
        stmts ++= innerStmts
      } else {
        stmts += IR_IfCondition(
          IR_IsValidComputationPoint(unknowns(i).field, Duplicate(unknowns(i).index)),
          innerStmts,
          boundaryStmts)
      }
    }

    /// solve local system

    // pre-compute inverse's of local sub-matrices
    def A11Inv = IR_VariableAccess("_local_A11Inv", IR_MatrixDatatype(IR_RealDatatype, 2, 2))

    def A22Inv = IR_VariableAccess("_local_A22Inv", IR_MatrixDatatype(IR_RealDatatype, 2, 2))

    def A33Inv = IR_VariableAccess("_local_A33Inv", IR_MatrixDatatype(IR_RealDatatype, 2, 2))

    // TODO: add return types
    stmts += IR_VariableDeclaration(A11Inv, IR_FunctionCall("inverse", A11))
    stmts += IR_VariableDeclaration(A22Inv, IR_FunctionCall("inverse", A22))
    stmts += IR_VariableDeclaration(A33Inv, IR_FunctionCall("inverse", A33))

    def S = IR_VariableAccess("_local_S", IR_MatrixDatatype(IR_RealDatatype, 1, 1))

    def GTilde = IR_VariableAccess("_local_GTilde", IR_MatrixDatatype(IR_RealDatatype, 1, 1))

    stmts += IR_VariableDeclaration(S, D - (C1 * A11Inv * B1 + C2 * A22Inv * B2 + C3 * A33Inv * B3))
    stmts += IR_VariableDeclaration(GTilde, G - C1 * A11Inv * F1 - C2 * A22Inv * F2 - C3 * A33Inv * F3)

    stmts += IR_Assignment(V, IR_FunctionCall("inverse", S) * GTilde)
    stmts += IR_Assignment(U1, A11Inv * (F1 - B1 * V))
    stmts += IR_Assignment(U2, A22Inv * (F2 - B2 * V))
    stmts += IR_Assignment(U3, A33Inv * (F3 - B3 * V))

    // write back results
    for (i <- unknowns.indices) {
      val dest = Duplicate(unknowns(i))
      if (jacobiType) dest.slot.asInstanceOf[IR_SlotAccess].offset += 1

      if (omitConditions) {
        if (relax.isEmpty)
          stmts += IR_Assignment(dest, vecComponentAccess(u(i), i % 2))
        else
          stmts += IR_Assignment(dest, Duplicate(unknowns(i)) * (1.0 - relax.get) + relax.get * vecComponentAccess(u(i), i % 2))
      } else {
        stmts += IR_IfCondition( // don't write back result on boundaries
          IR_IsValidComputationPoint(unknowns(i).field, Duplicate(unknowns(i).index)),
          if (relax.isEmpty)
            IR_Assignment(dest, vecComponentAccess(u(i), i % 2))
          else
            IR_Assignment(dest, Duplicate(unknowns(i)) * (1.0 - relax.get) + relax.get * vecComponentAccess(u(i), i % 2))
        )
      }
    }

    stmts
  }
}
