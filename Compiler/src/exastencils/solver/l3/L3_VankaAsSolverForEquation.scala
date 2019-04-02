package exastencils.solver.l3

import scala.collection.mutable._

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_ForLoop
import exastencils.config.Knowledge

/// L3_VankaAsSolverForEquation

object L3_VankaAsSolverForEquation extends L3_IterativeSolverForEquation {

  import L3_IterativeSolverForEquation._

  override def generateFor(entries : ListBuffer[L3_SolverForEqEntry], level : Int) = generateFor(entries, level, ListBuffer())
  def generateFor(entries : ListBuffer[L3_SolverForEqEntry], level : Int, smootherHints : ListBuffer[L3_GenerateSmootherHint]) = {
    // set up function for norm of residual
    generateResNormFunction(entries, level)

    val stmts = ListBuffer[L3_Statement]()

    // update residual
    stmts ++= entries.map(_.generateUpdateRes(level))

    // shortcuts for residual norm
    def curRes = L3_PlainVariableAccess("gen_curRes", L3_RealDatatype, false)

    def initRes = L3_PlainVariableAccess("gen_initRes", L3_RealDatatype, false)

    def nextRes = L3_PlainVariableAccess("gen_nextRes", L3_RealDatatype, false)

    def callResNorm = L3_FunctionCall(L3_LeveledDslFunctionReference("gen_resNorm", level, L3_RealDatatype))

    stmts += L3_VariableDeclaration(curRes, callResNorm)
    stmts += L3_VariableDeclaration(initRes, curRes)

    stmts += L3_IfCondition(curRes EqEq 0.0, L3_Return(None))

    // main loop
    def curStep = L3_PlainVariableAccess("gen_curStep", L3_IntegerDatatype, false)

    stmts += L3_VariableDeclaration(curStep, 0)

    val loopStmts = ListBuffer[L3_Statement]()

    val numInnerSteps = /* TODO: model as parameter */ 10
    loopStmts ++= L3_VankaForEquation.generateFor(entries, level, numInnerSteps, smootherHints)

    // update residual
    loopStmts ++= entries.map(_.generateUpdateRes(level))
    loopStmts += L3_VariableDeclaration(nextRes, callResNorm)

    // exit criterion
    val returnStmts = ListBuffer[L3_Statement]()
    if (L3_IterativeSolverForEquation.generateDebugPrints) {
      returnStmts += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype), ListBuffer[L3_Expression](
        L3_StringConstant("VankaAsSolver took"), curStep, L3_StringConstant("steps to reduce residual from"), initRes, L3_StringConstant("to"), curRes))
    }
    if (logCharacteristics)
      returnStmts += L3_FunctionCall(L3_PlainInternalFunctionReference("logCharacteristics", L3_UnitDatatype), L3_StringConstant("cgs_num_it"), curStep)
    returnStmts += L3_Return(None)

    loopStmts += L3_IfCondition(L3_LowerEqual(nextRes, Knowledge.solver_cgs_targetResReduction * initRes), returnStmts, ListBuffer())

    loopStmts += L3_Assignment(curRes, nextRes)

    stmts += L3_ForLoop(Knowledge.solver_cgs_maxNumIts, Some(curStep), loopStmts)

    stmts += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype),
      ListBuffer[L3_Expression](L3_StringConstant("Maximum number of cgs iterations ("), Knowledge.solver_cgs_maxNumIts, L3_StringConstant(") was exceeded")))
    if (logCharacteristics)
      stmts += L3_FunctionCall(L3_PlainInternalFunctionReference("logCharacteristics", L3_UnitDatatype), L3_StringConstant("cgs_num_it"), curStep)

    stmts
  }
}
