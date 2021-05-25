package exastencils.solver.ir

import exastencils.base.ir.{IR_Access, IR_ExpressionStatement, IR_FunctionCall, IR_VariableAccess}
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateRuntimeInversion
import exastencils.datastructures.{DefaultStrategy, QuietDefaultStrategy, Transformation}



object IR_InlineMatSolveStmts extends QuietDefaultStrategy("Resolve IR_LocalSolve nodes") {
  val MAT_SOLVE_STMT : String = MAT_SOLVE_STMT
  this += new Transformation("Find calls to solve statements and inline the corresponding function", {
    case _ @ IR_ExpressionStatement(fc @ IR_FunctionCall(_, args)) if fc.hasAnnotation(MAT_SOLVE_STMT) =>
        val m : Int = fc.removeAnnotation(MAT_SOLVE_STMT).get.asInstanceOf[Int]
        IR_MatrixSolveOps.genLUSolveInlined(
          args(0).asInstanceOf[IR_Access],
          m,
          args(1).asInstanceOf[IR_VariableAccess],
          args(2).asInstanceOf[IR_VariableAccess]
        )
  })
}
