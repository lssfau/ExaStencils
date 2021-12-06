package exastencils.solver.ir

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_ExpressionStatement
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_LinearizeMatrices
import exastencils.baseExt.ir.IR_PostItMOps
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation


object IR_InlineMatSolveStmts extends QuietDefaultStrategy("Resolve IR_LocalSolve nodes") {
  val MAT_SOLVE_STMT: String = "MAT_SOLVE_STMT"
  this += new Transformation("Find calls to solve statements and inline the corresponding function", {
    case _ @ IR_ExpressionStatement(fc @ IR_FunctionCall(_, args)) if fc.hasAnnotation(MAT_SOLVE_STMT) =>
      val m : Int = fc.removeAnnotation(MAT_SOLVE_STMT).get.asInstanceOf[Int]
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
