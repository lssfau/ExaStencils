package exastencils.solver.l3

import scala.collection.mutable._

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_ForLoop
import exastencils.boundary.l3.L3_NoBC
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.l3._

/// L3_ConjugateGradientForEquation

object L3_ConjugateGradientForEquation extends L3_IterativeSolverForEquation {

  import L3_IterativeSolverForEquation._

  override def generateFor(entries : ListBuffer[L3_SolverForEqEntry], level : Int) = {
    // set up function for norm of residual
    generateResNormFunction(entries, level)
    // set up temporary fields
    val tmp0 = HashMap[L3_SolverForEqEntry, L3_Field]()
    val tmp1 = HashMap[L3_SolverForEqEntry, L3_Field]()

    for (entry <- entries) {
      val res = entry.resPerLevel(level)

      val tmp0Field = L3_Field(s"gen_cgTmp0_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))
      val tmp1Field = L3_Field(s"gen_cgTmp1_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), L3_NoBC)

      L3_FieldCollection.add(tmp0Field)
      L3_FieldCollection.add(tmp1Field)

      tmp0 += (entry -> tmp0Field)
      tmp1 += (entry -> tmp1Field)
    }

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

    entries.foreach(entry =>
      stmts += L3_Assignment(L3_FieldAccess(tmp0(entry)), L3_FieldAccess(entry.resPerLevel(level))))

    // main loop
    def curStep = L3_PlainVariableAccess("gen_curStep", L3_IntegerDatatype, false)

    stmts += L3_VariableDeclaration(curStep, 0)

    val loopStmts = ListBuffer[L3_Statement]()

    // compose VecGradP = Stencil * VecP
    entries.foreach(entry =>
      loopStmts += generateOperatorApplication(tmp1(entry), entry.getEq(level).lhs, tmp0, entries, entry, level))

    // alpha calculation
    def alphaNom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_alphaNom_${ entry.getSolField(level).name }", L3_RealDatatype, false)
    def alphaDenom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_alphaDenom_${ entry.getSolField(level).name }", L3_RealDatatype, false)

    def alpha = L3_PlainVariableAccess("gen_alpha", L3_RealDatatype, false)
    def beta = L3_PlainVariableAccess("gen_beta", L3_RealDatatype, false)

    entries.foreach(entry => loopStmts += L3_VariableDeclaration(alphaNom(entry),
      L3_FieldFieldConvolution(L3_FieldAccess(entry.resPerLevel(level)), L3_FieldAccess(entry.resPerLevel(level)))))
    entries.foreach(entry => loopStmts += L3_VariableDeclaration(alphaDenom(entry),
      L3_FieldFieldConvolution(L3_FieldAccess(tmp0(entry)), L3_FieldAccess(tmp1(entry)))))

    loopStmts += L3_VariableDeclaration(alpha,
      entries.map(alphaNom(_) : L3_Expression).reduce(_ + _) / entries.map(alphaDenom(_) : L3_Expression).reduce(_ + _))

    entries.foreach(entry => {
      loopStmts += L3_Assignment(L3_FieldAccess(entry.getSolField(level)),
        alpha * L3_FieldAccess(tmp0(entry)), "+=", None)
      loopStmts += L3_Assignment(L3_FieldAccess(entry.resPerLevel(level)),
        alpha * L3_FieldAccess(tmp1(entry)), "-=", None)
    })

    loopStmts += L3_VariableDeclaration(nextRes, callResNorm)

    // exit criterion
    val returnStmts = ListBuffer[L3_Statement]()
    if (L3_IterativeSolverForEquation.generateDebugPrints) {
      returnStmts += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype), ListBuffer[L3_Expression](
        L3_StringConstant("CG took"), curStep, L3_StringConstant("steps to reduce residual from"), initRes, L3_StringConstant("to"), curRes))
    }
    returnStmts += L3_Return(None)

    loopStmts += L3_IfCondition(L3_LowerEqual(nextRes, Knowledge.solver_cgs_targetResReduction * initRes), returnStmts, ListBuffer())

    loopStmts += L3_VariableDeclaration(beta, (nextRes * nextRes) / (curRes * curRes))

    entries.foreach(entry =>
      loopStmts += L3_Assignment(L3_FieldAccess(tmp0(entry)),
        L3_FieldAccess(entry.resPerLevel(level)) + beta * L3_FieldAccess(tmp0(entry))))

    loopStmts += L3_Assignment(curRes, nextRes)

    stmts += L3_ForLoop(Knowledge.solver_cgs_maxNumIts, Some(curStep), loopStmts)

    stmts += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype),
      ListBuffer[L3_Expression](L3_StringConstant("Maximum number of cgs iterations ("), Knowledge.solver_cgs_maxNumIts, L3_StringConstant(") was exceeded")))

    stmts
  }
}
