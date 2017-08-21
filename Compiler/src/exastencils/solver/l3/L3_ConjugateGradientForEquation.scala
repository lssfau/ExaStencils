package exastencils.solver.l3

import scala.collection.mutable._

import exastencils.base.ExaRootNode
import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_ForLoop
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.l3._
import exastencils.util.l3.L3_MathFunctionReference

/// L3_ConjugateGradientForEquation

object L3_ConjugateGradientForEquation {
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
    val tmp0 = HashMap[L3_SolverForEqEntry, L3_Field]()
    val tmp1 = HashMap[L3_SolverForEqEntry, L3_Field]()

    for (entry <- entries) {
      val res = entry.resPerLevel(level)

      val tmp0Field = L3_Field(s"gen_cgTmp0_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))
      val tmp1Field = L3_Field(s"gen_cgTmp1_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))

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

    entries.foreach(entry =>
      stmts += L3_Assignment(L3_FieldAccess(tmp0(entry)), L3_FieldAccess(entry.resPerLevel(level))))

    // main loop
    def cgSteps = L3_PlainVariableAccess("gen_cgSteps", L3_IntegerDatatype, false)
    def maxSteps = 512

    stmts += L3_VariableDeclaration(cgSteps, 0)

    val loopStmts = ListBuffer[L3_Statement]()

    // compose VecGradP = Stencil * VecP
    for (entry <- entries) {
      val assignment = L3_Assignment(L3_FieldAccess(tmp1(entry)), Duplicate(entry.getEq(level).lhs))

      object L3_ReplaceAccesses extends QuietDefaultStrategy("Local replace of field accesses with temporary fields") {
        this += new Transformation("Search and replace", {
          case access @ L3_FieldAccess(field, _) if entries.exists(field == _.getSolField(level)) =>
            access.target = tmp0(entries.find(field == _.getSolField(level)).get)
            access
        })
      }

      L3_ReplaceAccesses.applyStandalone(assignment)

      loopStmts += assignment
    }

    // alpha calculation
    def alphaNom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_alphaNom_${ entry.getSolField(level).name }", L3_RealDatatype, false)
    def alphaDenom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_alphaDenom_${ entry.getSolField(level).name }", L3_RealDatatype, false)

    def alpha = L3_PlainVariableAccess("gen_alpha", L3_RealDatatype, false)
    def beta = L3_PlainVariableAccess("gen_beta", L3_RealDatatype, false)

    entries.foreach(entry => {
      loopStmts += L3_VariableDeclaration(alphaNom(entry), L3_FieldAccess(entry.resPerLevel(level)) * L3_FieldAccess(entry.resPerLevel(level)))
      loopStmts += L3_VariableDeclaration(alphaDenom(entry), L3_FieldAccess(tmp0(entry)) * L3_FieldAccess(tmp1(entry)))
    })

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
    loopStmts += L3_IfCondition(L3_LowerEqual(nextRes, 0.001 * initRes), ListBuffer[L3_Statement](L3_Return(None)), ListBuffer())

    loopStmts += L3_VariableDeclaration(beta, (nextRes * nextRes) / (curRes * curRes))

    entries.foreach(entry =>
      loopStmts += L3_Assignment(L3_FieldAccess(tmp0(entry)),
        L3_FieldAccess(entry.resPerLevel(level)) + beta * L3_FieldAccess(tmp0(entry))))

    loopStmts += L3_Assignment(curRes, nextRes)

    stmts += L3_ForLoop(maxSteps, Some(cgSteps), loopStmts)

    stmts += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype),
      ListBuffer[L3_Expression](L3_StringConstant("Maximum number of cgs iterations ("), maxSteps, L3_StringConstant(") was exceeded")))

    stmts
  }
}