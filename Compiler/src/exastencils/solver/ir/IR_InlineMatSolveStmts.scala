package exastencils.solver.ir

import exastencils.base.ir.{IR_Access, IR_Assignment, IR_ExpressionStatement, IR_FunctionCall, IR_Multiplication, IR_RealDatatype, IR_Scope, IR_VariableAccess, IR_VariableDeclaration}
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateRuntimeInversion
import exastencils.baseExt.ir.{IR_LinearizeMatrices, IR_MatrixDatatype, IR_PostItMOps}
import exastencils.datastructures.{DefaultStrategy, QuietDefaultStrategy, Transformation}


object IR_InlineMatSolveStmts extends QuietDefaultStrategy("Resolve IR_LocalSolve nodes") {
  val MAT_SOLVE_STMT: String = "MAT_SOLVE_STMT"
  this += new Transformation("Find calls to solve statements and inline the corresponding function", {
    case _@IR_ExpressionStatement(fc@IR_FunctionCall(_, args)) if fc.hasAnnotation(MAT_SOLVE_STMT) =>
      val m: Int = fc.removeAnnotation(MAT_SOLVE_STMT).get.asInstanceOf[Int]
      val inlinedSolveStmts = IR_MatrixSolveOps.genLUSolveInlined(
        args(0).asInstanceOf[IR_Access],
        m,
        args(1).asInstanceOf[IR_VariableAccess],
        args(2).asInstanceOf[IR_VariableAccess]
      )
      IR_PostItMOps.applyStandalone(inlinedSolveStmts)
      IR_LinearizeMatrices.applyStandalone(inlinedSolveStmts)
      inlinedSolveStmts
  })
}
