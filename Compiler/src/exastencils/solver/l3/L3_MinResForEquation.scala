package exastencils.solver.l3

import scala.collection.mutable._

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_ForLoop
import exastencils.boundary.l3.L3_NoBC
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.l3._
import exastencils.util.l3.L3_MathFunctionReference

/// L3_MinResForEquation

object L3_MinResForEquation extends L3_IterativeSolverForEquation {
  // following "Numerik linearer Gleichungssysteme: Direkte und iterative Verfahren" by C. Kanzow
  //           chapter 6, page 247, alg. 6.19

  import L3_IterativeSolverForEquation._

  override def generateFor(entries : ListBuffer[L3_SolverForEqEntry], level : Int) = {
    // set up function for norm of residual
    generateResNormFunction(entries, level)
    // set up temporary fields
    // FIXME: use slots instead
    val pOld = HashMap[L3_SolverForEqEntry, L3_Field]()
    val p = HashMap[L3_SolverForEqEntry, L3_Field]()
    val pNew = HashMap[L3_SolverForEqEntry, L3_Field]()

    val vOld = HashMap[L3_SolverForEqEntry, L3_Field]()
    val v = HashMap[L3_SolverForEqEntry, L3_Field]()
    val vNew = HashMap[L3_SolverForEqEntry, L3_Field]()

    // FIXME: eliminate av
    val av = HashMap[L3_SolverForEqEntry, L3_Field]()

    for (entry <- entries) {
      val res = entry.resPerLevel(level)

      val pOldField = L3_Field(s"gen_pOld_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), L3_NoBC)
      val pField = L3_Field(s"gen_p_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), L3_NoBC)
      val pNewField = L3_Field(s"gen_pNew_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), L3_NoBC)

      val vOldField = L3_Field(s"gen_vOld_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), L3_NoBC)
      val vField = L3_Field(s"gen_v_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))
      val vNewField = L3_Field(s"gen_vNew_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), L3_NoBC)

      val avField = L3_Field(s"gen_av_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), L3_NoBC)

      L3_FieldCollection.add(pOldField)
      L3_FieldCollection.add(pField)
      L3_FieldCollection.add(pNewField)

      L3_FieldCollection.add(vOldField)
      L3_FieldCollection.add(vField)
      L3_FieldCollection.add(vNewField)

      L3_FieldCollection.add(avField)

      pOld += (entry -> pOldField)
      p += (entry -> pField)
      pNew += (entry -> pNewField)

      vOld += (entry -> vOldField)
      v += (entry -> vField)
      vNew += (entry -> vNewField)

      av += (entry -> avField)
    }

    val stmts = ListBuffer[L3_Statement]()

    // update residual
    stmts ++= entries.map(_.generateUpdateRes(level))

    // shortcuts for residual norm
    def curRes = L3_PlainVariableAccess("gen_curRes", L3_RealDatatype, false)
    def initRes = L3_PlainVariableAccess("gen_initRes", L3_RealDatatype, false)
    def callResNorm = L3_FunctionCall(L3_LeveledDslFunctionReference("gen_resNorm", level, L3_RealDatatype))

    stmts += L3_VariableDeclaration(curRes, callResNorm)
    stmts += L3_VariableDeclaration(initRes, curRes)

    stmts += L3_IfCondition(curRes <= Knowledge.solver_cgs_absResThreshold, L3_Return(None))

    // init fields
    entries.foreach(entry => stmts += L3_Assignment(L3_FieldAccess(p(entry)), 0.0))
    entries.foreach(entry => stmts += L3_Assignment(L3_FieldAccess(pNew(entry)), 0.0))
    entries.foreach(entry => stmts += L3_Assignment(L3_FieldAccess(v(entry)), 0.0))
    entries.foreach(entry =>
      stmts += L3_Assignment(L3_FieldAccess(vNew(entry)), L3_FieldAccess(entry.resPerLevel(level)) / initRes))

    // init variables

    def beta = L3_PlainVariableAccess("gen_beta", L3_RealDatatype, false)
    def betaNew = L3_PlainVariableAccess("gen_betaNew", L3_RealDatatype, false)
    def cOld = L3_PlainVariableAccess("gen_cOld", L3_RealDatatype, false)
    def c = L3_PlainVariableAccess("gen_c", L3_RealDatatype, false)
    def cNew = L3_PlainVariableAccess("gen_cNew", L3_RealDatatype, false)
    def sOld = L3_PlainVariableAccess("gen_sOld", L3_RealDatatype, false)
    def s = L3_PlainVariableAccess("gen_s", L3_RealDatatype, false)
    def sNew = L3_PlainVariableAccess("gen_sNew", L3_RealDatatype, false)

    stmts += L3_VariableDeclaration(beta, 0.0)
    stmts += L3_VariableDeclaration(betaNew, 0.0)
    stmts += L3_VariableDeclaration(cOld, 1.0)
    stmts += L3_VariableDeclaration(c, 1.0)
    stmts += L3_VariableDeclaration(cNew, 1.0)
    stmts += L3_VariableDeclaration(sOld, 0.0)
    stmts += L3_VariableDeclaration(s, 0.0)
    stmts += L3_VariableDeclaration(sNew, 0.0)

    // main loop
    def curStep = L3_PlainVariableAccess("gen_curStep", L3_IntegerDatatype, false)

    stmts += L3_VariableDeclaration(curStep, 0)

    val loopStmts = ListBuffer[L3_Statement]()

    if (Knowledge.solver_cgs_restart) {
      loopStmts += L3_IfCondition((curStep > 0) AndAnd (0 EqEq (curStep Mod Knowledge.solver_cgs_restartAfter)),
        entries.map(_.generateUpdateRes(level)) ++
          ListBuffer(L3_Assignment(curRes, callResNorm)) ++
          entries.map(entry => L3_Assignment(L3_FieldAccess(p(entry)), 0.0)) ++
          entries.map(entry => L3_Assignment(L3_FieldAccess(pNew(entry)), 0.0)) ++
          entries.map(entry => L3_Assignment(L3_FieldAccess(v(entry)), 0.0)) ++
          entries.map(entry => L3_Assignment(L3_FieldAccess(vNew(entry)), L3_FieldAccess(entry.resPerLevel(level)) / curRes)) ++
          ListBuffer[L3_Statement](
            L3_Assignment(beta, 0.0),
            L3_Assignment(betaNew, 0.0),
            L3_Assignment(cOld, 1.0),
            L3_Assignment(c, 1.0),
            L3_Assignment(cNew, 1.0),
            L3_Assignment(sOld, 0.0),
            L3_Assignment(s, 0.0),
            L3_Assignment(sNew, 0.0))
      )
    }

    // shift v and beta
    loopStmts += L3_Assignment(beta, betaNew)
    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(vOld(entry)), L3_FieldAccess(v(entry))))
    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(v(entry)), L3_FieldAccess(vNew(entry))))

    // compute av and vNew
    entries.foreach(entry =>
      loopStmts += generateOperatorApplication(av(entry), entry.getEq(level).lhs, v, entries, entry, level))

    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(vNew(entry)), L3_FieldAccess(av(entry)) - beta * L3_FieldAccess(vOld(entry))))

    // alpha calculation
    def alpha = L3_PlainVariableAccess("gen_alpha", L3_RealDatatype, false)

    loopStmts += L3_VariableDeclaration(alpha, 0.0)
    entries.foreach(entry => loopStmts += L3_Assignment(alpha,
      L3_FieldFieldConvolution(L3_FieldAccess(vNew(entry)), L3_FieldAccess(v(entry))), "+=", None))

    // update vNew
    entries.foreach(entry =>
      loopStmts += L3_Assignment(L3_FieldAccess(vNew(entry)), alpha * L3_FieldAccess(v(entry)), "-=", None))

    loopStmts += L3_Assignment(betaNew, 0.0)
    entries.foreach(entry => loopStmts += L3_Assignment(betaNew,
      L3_FieldFieldConvolution(L3_FieldAccess(vNew(entry)), L3_FieldAccess(vNew(entry))), "+=", None))
    loopStmts += L3_Assignment(betaNew, L3_FunctionCall(L3_MathFunctionReference.sqrt, betaNew))

    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(vNew(entry)), betaNew, "/=", None))

    // update c, s, rho

    loopStmts += L3_Assignment(cOld, c)
    loopStmts += L3_Assignment(c, cNew)

    loopStmts += L3_Assignment(sOld, s)
    loopStmts += L3_Assignment(s, sNew)

    def rho1 = L3_PlainVariableAccess("rho1", L3_RealDatatype, false)
    def rho2 = L3_PlainVariableAccess("rho2", L3_RealDatatype, false)
    def rho3 = L3_PlainVariableAccess("rho3", L3_RealDatatype, false)
    def rho3Tilde = L3_PlainVariableAccess("rho3Tilde", L3_RealDatatype, false)

    loopStmts += L3_VariableDeclaration(rho1, sOld * beta)
    loopStmts += L3_VariableDeclaration(rho2, c * cOld * beta + s * alpha)
    loopStmts += L3_VariableDeclaration(rho3Tilde, c * alpha - s * cOld * beta)

    // update tau and nu

    def tau = L3_PlainVariableAccess("tau", L3_RealDatatype, false)
    def nu = L3_PlainVariableAccess("nu", L3_RealDatatype, false)

    loopStmts += L3_VariableDeclaration(tau, L3_FunctionCall(L3_MathFunctionReference.fabs, rho3Tilde) + L3_FunctionCall(L3_MathFunctionReference.fabs, betaNew))
    loopStmts += L3_VariableDeclaration(nu, tau * L3_FunctionCall(L3_MathFunctionReference.sqrt, (rho3Tilde / tau) * (rho3Tilde / tau) + (betaNew / tau) * (betaNew / tau)))

    // update c, s and rho

    loopStmts += L3_Assignment(cNew, rho3Tilde / nu)
    loopStmts += L3_Assignment(sNew, betaNew / nu)
    loopStmts += L3_VariableDeclaration(rho3, nu)

    // update p
    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(pOld(entry)), L3_FieldAccess(p(entry))))
    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(p(entry)), L3_FieldAccess(pNew(entry))))
    entries.foreach(entry =>
      loopStmts += L3_Assignment(L3_FieldAccess(pNew(entry)), (L3_FieldAccess(v(entry)) - rho1 * L3_FieldAccess(pOld(entry)) - rho2 * L3_FieldAccess(p(entry))) / rho3))

    // update solution
    entries.foreach(entry =>
      loopStmts += L3_Assignment(L3_FieldAccess(entry.getSolField(level)), cNew * curRes * L3_FieldAccess(pNew(entry)), "+=", None))

    // exit criterion
    loopStmts += L3_Assignment(curRes, -1.0 * sNew * curRes)

    val returnStmts = ListBuffer[L3_Statement]()
    if (L3_IterativeSolverForEquation.generateDebugPrints) {
      returnStmts += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype), ListBuffer[L3_Expression](
        L3_StringConstant("MinRes took"), curStep, L3_StringConstant("steps to reduce residual from"), initRes, L3_StringConstant("to"), curRes))
    }
    returnStmts += L3_Return(None)

    loopStmts += L3_IfCondition((L3_FunctionCall(L3_MathFunctionReference.fabs, curRes) <= Knowledge.solver_cgs_targetResReduction * initRes)
      OrOr (L3_FunctionCall(L3_MathFunctionReference.fabs, curRes) <= Knowledge.solver_cgs_absResThreshold), returnStmts, ListBuffer())

    // compile loop
    stmts += L3_ForLoop(Knowledge.solver_cgs_maxNumIts, Some(curStep), loopStmts)

    stmts += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype), ListBuffer[L3_Expression](
      L3_StringConstant("Maximum number of cgs iterations ("), Knowledge.solver_cgs_maxNumIts, L3_StringConstant(") was exceeded")))

    stmts
  }
}
