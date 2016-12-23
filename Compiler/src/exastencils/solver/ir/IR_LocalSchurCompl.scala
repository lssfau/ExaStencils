package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.boundary.ir.IR_IsValidComputationPoint
import exastencils.core.Duplicate
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger

/// IR_LocalSchurCompl

object IR_LocalSchurCompl {
  def apply(AVals : ListBuffer[ListBuffer[IR_Addition]], fVals : ListBuffer[IR_Addition], unknowns : ListBuffer[IR_FieldAccess]) =
    invert(AVals, fVals, unknowns)

  def suitable(AVals : ListBuffer[ListBuffer[IR_Addition]]) : Boolean = {
    // TODO: currently assumes special case of 3D velocity-pressure coupling

    // check matrix dimensions
    if (7 != AVals.size)
      return false

    var onlyZeros = true
    for (i <- 0 until 7; j <- 0 until 7; if (
      i != 6 // ignore last row
        && j != 6 // ignore last column
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

  def invert(AVals : ListBuffer[ListBuffer[IR_Addition]], fVals : ListBuffer[IR_Addition], unknowns : ListBuffer[IR_FieldAccess]) : ListBuffer[IR_Statement] = {
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
        case 0 | 1 => innerStmts += IR_Assignment(IR_HackVecComponentAccess(f(i), i - 0), fVals(i))
        case 2 | 3 => innerStmts += IR_Assignment(IR_HackVecComponentAccess(f(i), i - 2), fVals(i))
        case 4 | 5 => innerStmts += IR_Assignment(IR_HackVecComponentAccess(f(i), i - 4), fVals(i))
        case 6     => innerStmts += IR_Assignment(IR_HackVecComponentAccess(f(i), i - 6), fVals(i))
      }

      // sub-matrices for inner
      // TODO: check for non-zero entries that are discarded
      i match {
        case 0 | 1 =>
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(A11, i - 0, 0), AVals(i)(0))
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(A11, i - 0, 1), AVals(i)(1))
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(B1, i - 0, 0), AVals(i)(6))
        case 2 | 3 =>
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(A22, i - 2, 0), AVals(i)(2))
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(A22, i - 2, 1), AVals(i)(3))
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(B2, i - 2, 0), AVals(i)(6))
        case 4 | 5 =>
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(A33, i - 4, 0), AVals(i)(4))
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(A33, i - 4, 1), AVals(i)(5))
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(B3, i - 4, 0), AVals(i)(6))
        case 6     =>
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(C1, 0, 0 - 0), AVals(i)(0))
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(C1, 0, 1 - 0), AVals(i)(1))
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(C2, 0, 2 - 2), AVals(i)(2))
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(C2, 0, 3 - 2), AVals(i)(3))
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(C3, 0, 4 - 4), AVals(i)(4))
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(C3, 0, 5 - 4), AVals(i)(5))
          innerStmts += IR_Assignment(IR_HackMatComponentAccess(D, i - 6, 0), AVals(i)(6))
      }

      // rhs for boundary
      boundaryStmts += IR_Assignment(IR_HackVecComponentAccess(f(i), i % 2), Duplicate(unknowns(i)))

      // sub-matrices for inner
      i match {
        case 0 | 1 =>
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(A11, i - 0, 0), if (0 == i - 0) 1 else 0)
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(A11, i - 0, 1), if (1 == i - 0) 1 else 0)
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(B1, i - 0, 0), 0.0)
        case 2 | 3 =>
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(A22, i - 2, 0), if (0 == i - 2) 1 else 0)
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(A22, i - 2, 1), if (1 == i - 2) 1 else 0)
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(B2, i - 2, 0), 0.0)
        case 4 | 5 =>
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(A33, i - 4, 0), if (0 == i - 4) 1 else 0)
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(A33, i - 4, 1), if (1 == i - 4) 1 else 0)
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(B3, i - 4, 0), 0.0)
        case 6     =>
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(C1, 0, 0 - 0), 0.0)
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(C1, 0, 1 - 0), 0.0)
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(C2, 0, 2 - 2), 0.0)
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(C2, 0, 3 - 2), 0.0)
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(C3, 0, 4 - 4), 0.0)
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(C3, 0, 5 - 4), 0.0)
          boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(D, i - 6, 0), 1.0)
      }

      // implement check if current unknown is on/ beyond boundary
      stmts += IR_IfCondition(
        IR_IsValidComputationPoint(Duplicate(unknowns(i).fieldSelection), Duplicate(unknowns(i).index)),
        innerStmts,
        boundaryStmts)
    }

    /// solve local system

    // pre-compute inverse's of local sub-matrices
    def A11Inv = IR_VariableAccess("_local_A11Inv", IR_MatrixDatatype(IR_RealDatatype, 2, 2))
    def A22Inv = IR_VariableAccess("_local_A22Inv", IR_MatrixDatatype(IR_RealDatatype, 2, 2))
    def A33Inv = IR_VariableAccess("_local_A33Inv", IR_MatrixDatatype(IR_RealDatatype, 2, 2))

    stmts += IR_VariableDeclaration(A11Inv, IR_MemberFunctionCall(A11, "inverse"))
    stmts += IR_VariableDeclaration(A22Inv, IR_MemberFunctionCall(A22, "inverse"))
    stmts += IR_VariableDeclaration(A33Inv, IR_MemberFunctionCall(A33, "inverse"))

    def S = IR_VariableAccess("_local_S", IR_MatrixDatatype(IR_RealDatatype, 1, 1))
    def GTilde = IR_VariableAccess("_local_GTilde", IR_MatrixDatatype(IR_RealDatatype, 1, 1))

    stmts += IR_VariableDeclaration(S, D - (C1 * A11Inv * B1 + C2 * A22Inv * B2 + C3 * A33Inv * B3))
    stmts += IR_VariableDeclaration(GTilde, G - C1 * A11Inv * F1 - C2 * A22Inv * F2 - C3 * A33Inv * F3)

    stmts += IR_Assignment(V, IR_MemberFunctionCall(S, "inverse") * GTilde)
    stmts += IR_Assignment(U1, A11Inv * (F1 - B1 * V))
    stmts += IR_Assignment(U2, A22Inv * (F2 - B2 * V))
    stmts += IR_Assignment(U3, A33Inv * (F3 - B3 * V))

    // write back results
    for (i <- unknowns.indices)
      stmts += IR_IfCondition(// don't write back result on boundaries
        IR_IsValidComputationPoint(Duplicate(unknowns(i).fieldSelection), Duplicate(unknowns(i).index)),
        IR_Assignment(Duplicate(unknowns(i)), IR_HackVecComponentAccess(u(i), i % 2)))

    stmts
  }
}