package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.boundary.ir.IR_IsValidComputationPoint
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.ir.IR_FieldAccess

/// IR_LocalDirectInvert

object IR_LocalDirectInvert {
  // TODO: remove after successful integration of experimental_internalHighDimTypes
  def vecComponentAccess(vec : IR_VariableAccess, i0 : Int) = {
    if (Knowledge.experimental_internalHighDimTypes)
      IR_HighDimAccess(vec, IR_ConstIndex(i0))
    else
      IR_HackVecComponentAccess(vec, i0)
  }

  // TODO: remove after successful integration of experimental_internalHighDimTypes
  def matComponentAccess(mat : IR_VariableAccess, i0 : Int, i1 : Int) = {
    if (Knowledge.experimental_internalHighDimTypes)
      IR_HighDimAccess(mat, IR_ConstIndex(i0, i1))
    else
      IR_HackMatComponentAccess(mat, i0, i1)
  }

  def apply(AVals : ListBuffer[ListBuffer[IR_Addition]], fVals : ListBuffer[IR_Addition], unknowns : ListBuffer[IR_FieldAccess],
      relax : Option[IR_Expression]) =
    invert(AVals, fVals, unknowns, relax)

  def invert(AVals : ListBuffer[ListBuffer[IR_Addition]], fVals : ListBuffer[IR_Addition], unknowns : ListBuffer[IR_FieldAccess],
      relax : Option[IR_Expression]) : ListBuffer[IR_Statement] = {
    val stmts = ListBuffer[IR_Statement]()

    def u =
      if (Knowledge.experimental_internalHighDimTypes)
        IR_VariableAccess("_local_unknowns", IR_MatrixDatatype(IR_RealDatatype, unknowns.length, 1))
      else
        IR_VariableAccess("_local_unknowns", IR_VectorDatatype(IR_RealDatatype, unknowns.length, Some(false)))

    def f =
      if (Knowledge.experimental_internalHighDimTypes)
        IR_VariableAccess("_local_rhs", IR_MatrixDatatype(IR_RealDatatype, unknowns.length, 1))
      else
        IR_VariableAccess("_local_rhs", IR_VectorDatatype(IR_RealDatatype, unknowns.length, Some(false)))

    def A = IR_VariableAccess("_local_matrix", IR_MatrixDatatype(IR_RealDatatype, unknowns.length, unknowns.length))

    // declare local variables -> to be merged later
    stmts += IR_VariableDeclaration(u)
    stmts += IR_VariableDeclaration(f)
    stmts += IR_VariableDeclaration(A)

    if (Knowledge.experimental_internalHighDimTypes) {
      // TODO: replace 0 with correct value/type
      stmts += IR_Assignment(u, 0)
      stmts += IR_Assignment(f, 0)
      stmts += IR_Assignment(A, 0)
    } else {
      stmts += IR_MemberFunctionCall(u, "set", ListBuffer[IR_Expression](0))
      stmts += IR_MemberFunctionCall(f, "set", ListBuffer[IR_Expression](0))
      stmts += IR_MemberFunctionCall(A, "set", ListBuffer[IR_Expression](0))
    }

    // construct rhs and matrix
    for (i <- unknowns.indices) {
      var innerStmts = ListBuffer[IR_Statement]()
      var boundaryStmts = ListBuffer[IR_Statement]()

      innerStmts += IR_Assignment(vecComponentAccess(f, i), fVals(i))
      for (j <- unknowns.indices)
        innerStmts += IR_Assignment(matComponentAccess(A, i, j), AVals(i)(j))

      boundaryStmts += IR_Assignment(vecComponentAccess(f, i), Duplicate(unknowns(i)))
      for (j <- unknowns.indices)
        boundaryStmts += IR_Assignment(matComponentAccess(A, i, j), if (i == j) 1 else 0)

      // check if current unknown is on/ beyond boundary
      stmts += IR_IfCondition(
        IR_IsValidComputationPoint(Duplicate(unknowns(i).fieldSelection), Duplicate(unknowns(i).index)),
        innerStmts,
        boundaryStmts)
    }

    // solve local system - TODO: replace inverse function call with internal function
    if (Knowledge.experimental_internalHighDimTypes)
    // TODO: set return value of the fct call
      stmts += IR_Assignment(u, IR_Multiplication(IR_FunctionCall("inverse", A), f))
    else
      stmts += IR_Assignment(u, IR_Multiplication(IR_MemberFunctionCall(A, "inverse"), f))

    // write back results
    for (i <- unknowns.indices)
      stmts += IR_IfCondition( // don't write back result on boundaries
        IR_IsValidComputationPoint(Duplicate(unknowns(i).fieldSelection), Duplicate(unknowns(i).index)),
        if (relax.isEmpty)
          IR_Assignment(Duplicate(unknowns(i)), vecComponentAccess(u, i))
        else
          IR_Assignment(Duplicate(unknowns(i)), Duplicate(unknowns(i)) * (1.0 - relax.get) + relax.get * vecComponentAccess(u, i))
      )

    stmts
  }
}