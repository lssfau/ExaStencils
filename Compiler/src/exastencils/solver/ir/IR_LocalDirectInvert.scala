package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.boundary.ir.IR_IsValidComputationPoint
import exastencils.field.ir.IR_FieldAccess

/// IR_LocalDirectInvert

object IR_LocalDirectInvert {
  def apply(AVals : ListBuffer[ListBuffer[IR_Addition]], fVals : ListBuffer[IR_Addition], unknowns : ListBuffer[IR_FieldAccess]) =
    invert(AVals, fVals, unknowns)

  def invert(AVals : ListBuffer[ListBuffer[IR_Addition]], fVals : ListBuffer[IR_Addition], unknowns : ListBuffer[IR_FieldAccess]) : ListBuffer[IR_Statement] = {
    val stmts = ListBuffer[IR_Statement]()

    def u = IR_VariableAccess("_local_unknowns", IR_VectorDatatype(IR_RealDatatype, unknowns.length, Some(false)))
    def f = IR_VariableAccess("_local_rhs", IR_VectorDatatype(IR_RealDatatype, unknowns.length, Some(false)))
    def A = IR_VariableAccess("_local_matrix", IR_MatrixDatatype(IR_RealDatatype, unknowns.length, unknowns.length))

    // declare local variables -> to be merged later
    stmts += IR_VariableDeclaration(u)
    stmts += IR_VariableDeclaration(f)
    stmts += IR_VariableDeclaration(A)

    // initialize with zero - TODO: adapt to new matrix types
    stmts += IR_MemberFunctionCall(u, "set", ListBuffer[IR_Expression](0))
    stmts += IR_MemberFunctionCall(f, "set", ListBuffer[IR_Expression](0))
    stmts += IR_MemberFunctionCall(A, "set", ListBuffer[IR_Expression](0))

    // construct rhs and matrix
    for (i <- unknowns.indices) {
      var innerStmts = ListBuffer[IR_Statement]()
      var boundaryStmts = ListBuffer[IR_Statement]()

      innerStmts += IR_Assignment(IR_HackVecComponentAccess(f, i), fVals(i))
      for (j <- unknowns.indices)
        innerStmts += IR_Assignment(IR_HackMatComponentAccess(A, i, j), AVals(i)(j))

      boundaryStmts += IR_Assignment(IR_HackVecComponentAccess(f, i), unknowns(i))
      for (j <- unknowns.indices)
        boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(A, i, j), if (i == j) 1 else 0)

      // check if current unknown is on/ beyond boundary
      stmts += IR_IfCondition(
        IR_IsValidComputationPoint(unknowns(i).fieldSelection, unknowns(i).index),
        innerStmts,
        boundaryStmts)
    }

    // solve local system - TODO: replace inverse function call with internal function
    stmts += IR_Assignment(u, IR_Multiplication(IR_MemberFunctionCall(A, "inverse"), f))

    // write back results
    for (i <- unknowns.indices)
      stmts += IR_IfCondition(// don't write back result on boundaries
        IR_IsValidComputationPoint(unknowns(i).fieldSelection, unknowns(i).index),
        IR_Assignment(unknowns(i), IR_HackVecComponentAccess(u, i)))

    stmts
  }
}