package exastencils.solver.l3

import scala.collection.mutable._

import exastencils.base.ExaRootNode
import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_ForLoop
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.l3._
import exastencils.util.l3.L3_MathFunctionReference

/// L3_MinResForEquation

object L3_MinResForEquation {
  def generateResNormFunction(entries : ListBuffer[L3_SolverForEqEntry], level : Int) = {
    val fctBody = ListBuffer[L3_Statement]()

    def resNorm = L3_PlainVariableAccess("gen_resNorm", L3_RealDatatype, false)
    fctBody += L3_VariableDeclaration(resNorm, 0.0)

    entries.foreach(entry =>
      fctBody += L3_Assignment(resNorm,
        L3_FieldAccess(entry.resPerLevel(level)) * L3_FieldAccess(entry.resPerLevel(level)),
        "+=", None))

    fctBody += L3_Return(Some(L3_FunctionCall(L3_MathFunctionReference("sqrt", L3_RealDatatype), resNorm)))

    val fct = L3_LeveledFunction(s"gen_resNorm", level, L3_RealDatatype, ListBuffer(), fctBody)
    ExaRootNode.l3_root.nodes += fct
  }

  def generateFor(entries : ListBuffer[L3_SolverForEqEntry], level : Int) = {
    // set up function for norm of residual
    generateResNormFunction(entries, level)
    // set up temporary fields
    // FIXME: use slots instead
    val p0 = HashMap[L3_SolverForEqEntry, L3_Field]()
    val s0 = HashMap[L3_SolverForEqEntry, L3_Field]()
    val p1 = HashMap[L3_SolverForEqEntry, L3_Field]()
    val s1 = HashMap[L3_SolverForEqEntry, L3_Field]()
    val p2 = HashMap[L3_SolverForEqEntry, L3_Field]()
    val s2 = HashMap[L3_SolverForEqEntry, L3_Field]()

    for (entry <- entries) {
      val res = entry.resPerLevel(level)

      val p0Field = L3_Field(s"gen_p0_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))
      val s0Field = L3_Field(s"gen_s0_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))
      val p1Field = L3_Field(s"gen_p1_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))
      val s1Field = L3_Field(s"gen_s1_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))
      val p2Field = L3_Field(s"gen_p2_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))
      val s2Field = L3_Field(s"gen_s2_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))

      L3_FieldCollection.add(p0Field)
      L3_FieldCollection.add(s0Field)
      L3_FieldCollection.add(p1Field)
      L3_FieldCollection.add(s1Field)
      L3_FieldCollection.add(p2Field)
      L3_FieldCollection.add(s2Field)

      p0 += (entry -> p0Field)
      s0 += (entry -> s0Field)
      p1 += (entry -> p1Field)
      s1 += (entry -> s1Field)
      p2 += (entry -> p2Field)
      s2 += (entry -> s2Field)
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

    // init p0 and s0
    entries.foreach(entry =>
      stmts += L3_Assignment(L3_FieldAccess(p0(entry)), L3_FieldAccess(entry.resPerLevel(level))))
    for (entry <- entries) {
      val assignment = L3_Assignment(L3_FieldAccess(s0(entry)), Duplicate(entry.getEq(level).lhs))

      object L3_ReplaceAccesses extends QuietDefaultStrategy("Local replace of field accesses with temporary fields") {
        this += new Transformation("Search and replace", {
          case access @ L3_FieldAccess(field, _) if entries.exists(field == _.getSolField(level)) =>
            access.target = p0(entries.find(field == _.getSolField(level)).get)
            access
        })
      }

      L3_ReplaceAccesses.applyStandalone(assignment)

      stmts += assignment
    }

    // init p1 and s1
    entries.foreach(entry => stmts += L3_Assignment(L3_FieldAccess(p1(entry)), L3_FieldAccess(p0(entry))))
    entries.foreach(entry => stmts += L3_Assignment(L3_FieldAccess(s1(entry)), L3_FieldAccess(s0(entry))))

    // main loop
    def cgsSteps = L3_PlainVariableAccess("gen_cgsSteps", L3_IntegerDatatype, false)

    stmts += L3_VariableDeclaration(cgsSteps, 0)

    val loopStmts = ListBuffer[L3_Statement]()

    // shift p and s
    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(p2(entry)), L3_FieldAccess(p1(entry))))
    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(p1(entry)), L3_FieldAccess(p0(entry))))
    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(s2(entry)), L3_FieldAccess(s1(entry))))
    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(s1(entry)), L3_FieldAccess(s0(entry))))

    // alpha calculation
    def alphaNom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_alphaNom_${ entry.getSolField(level).name }", L3_RealDatatype, false)
    def alphaDenom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_alphaDenom_${ entry.getSolField(level).name }", L3_RealDatatype, false)
    def alpha = L3_PlainVariableAccess("gen_alpha", L3_RealDatatype, false)

    entries.foreach(entry => {
      loopStmts += L3_VariableDeclaration(alphaNom(entry), L3_FieldAccess(entry.resPerLevel(level)) * L3_FieldAccess(s1(entry)))
      loopStmts += L3_VariableDeclaration(alphaDenom(entry), L3_FieldAccess(s1(entry)) * L3_FieldAccess(s1(entry)))
    })

    loopStmts += L3_VariableDeclaration(alpha,
      entries.map(alphaNom(_) : L3_Expression).reduce(_ + _) / entries.map(alphaDenom(_) : L3_Expression).reduce(_ + _))

    entries.foreach(entry => {
      loopStmts += L3_Assignment(L3_FieldAccess(entry.getSolField(level)),
        alpha * L3_FieldAccess(p1(entry)), "+=", None)
      loopStmts += L3_Assignment(L3_FieldAccess(entry.resPerLevel(level)),
        alpha * L3_FieldAccess(s1(entry)), "-=", None)
    })

    loopStmts += L3_Assignment(curRes, callResNorm)

    // exit criterion
    loopStmts += L3_IfCondition(L3_LowerEqual(curRes, Knowledge.solver_cgs_targetResReduction * initRes), ListBuffer[L3_Statement](L3_Return(None)), ListBuffer())

    // update p0 and s0
    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(p0(entry)), L3_FieldAccess(s1(entry))))

    for (entry <- entries) {
      val assignment = L3_Assignment(L3_FieldAccess(s0(entry)), Duplicate(entry.getEq(level).lhs))

      object L3_ReplaceAccesses extends QuietDefaultStrategy("Local replace of field accesses with temporary fields") {
        this += new Transformation("Search and replace", {
          case access @ L3_FieldAccess(field, _) if entries.exists(field == _.getSolField(level)) =>
            access.target = s1(entries.find(field == _.getSolField(level)).get)
            access
        })
      }

      L3_ReplaceAccesses.applyStandalone(assignment)

      loopStmts += assignment
    }

    // update beta1, p0 and s0

    def beta1Nom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_beta1Nom_${ entry.getSolField(level).name }", L3_RealDatatype, false)
    def beta1Denom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_beta1Denom_${ entry.getSolField(level).name }", L3_RealDatatype, false)
    def beta1 = L3_PlainVariableAccess("gen_beta1", L3_RealDatatype, false)

    entries.foreach(entry => {
      loopStmts += L3_VariableDeclaration(beta1Nom(entry), L3_FieldAccess(s0(entry)) * L3_FieldAccess(s1(entry)))
      loopStmts += L3_VariableDeclaration(beta1Denom(entry), L3_FieldAccess(s1(entry)) * L3_FieldAccess(s1(entry)))
    })

    loopStmts += L3_VariableDeclaration(beta1,
      entries.map(beta1Nom(_) : L3_Expression).reduce(_ + _) / entries.map(beta1Denom(_) : L3_Expression).reduce(_ + _))

    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(p0(entry)), beta1 * L3_FieldAccess(p1(entry)), "-=", None))
    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(s0(entry)), beta1 * L3_FieldAccess(s1(entry)), "-=", None))

    // update beta2, p0 and s0 if not in first iteration

    val condStmts = ListBuffer[L3_Statement]()

    def beta2Nom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_beta2Nom_${ entry.getSolField(level).name }", L3_RealDatatype, false)
    def beta2Denom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_beta2Denom_${ entry.getSolField(level).name }", L3_RealDatatype, false)
    def beta2 = L3_PlainVariableAccess("gen_beta2", L3_RealDatatype, false)

    entries.foreach(entry => {
      condStmts += L3_VariableDeclaration(beta2Nom(entry), L3_FieldAccess(s0(entry)) * L3_FieldAccess(s2(entry)))
      condStmts += L3_VariableDeclaration(beta2Denom(entry), L3_FieldAccess(s2(entry)) * L3_FieldAccess(s2(entry)))
    })

    condStmts += L3_VariableDeclaration(beta2,
      entries.map(beta2Nom(_) : L3_Expression).reduce(_ + _) / entries.map(beta2Denom(_) : L3_Expression).reduce(_ + _))

    entries.foreach(entry => condStmts += L3_Assignment(L3_FieldAccess(p0(entry)), beta2 * L3_FieldAccess(p2(entry)), "-=", None))
    entries.foreach(entry => condStmts += L3_Assignment(L3_FieldAccess(s0(entry)), beta2 * L3_FieldAccess(s2(entry)), "-=", None))

    loopStmts += L3_IfCondition(L3_Greater(cgsSteps, 0), condStmts, ListBuffer())

    stmts += L3_ForLoop(Knowledge.solver_cgs_maxNumIts, Some(cgsSteps), loopStmts)

    stmts += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype),
      ListBuffer[L3_Expression](L3_StringConstant("Maximum number of cgs iterations ("), Knowledge.solver_cgs_maxNumIts, L3_StringConstant(") was exceeded")))

    stmts
  }
}