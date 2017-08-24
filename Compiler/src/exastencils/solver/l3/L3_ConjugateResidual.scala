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

/// L3_ConjugateResidualForEquation

object L3_ConjugateResidualForEquation {
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
    val p = HashMap[L3_SolverForEqEntry, L3_Field]()
    val ap = HashMap[L3_SolverForEqEntry, L3_Field]()
    val ar = HashMap[L3_SolverForEqEntry, L3_Field]()

    for (entry <- entries) {
      val res = entry.resPerLevel(level)

      val pField = L3_Field(s"gen_p_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))
      val apField = L3_Field(s"gen_ap_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))
      val arField = L3_Field(s"gen_ar_${ entry.getSolField(level).name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))

      L3_FieldCollection.add(pField)
      L3_FieldCollection.add(apField)
      L3_FieldCollection.add(arField)

      p += (entry -> pField)
      ap += (entry -> apField)
      ar += (entry -> arField)
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
      stmts += L3_Assignment(L3_FieldAccess(p(entry)), L3_FieldAccess(entry.resPerLevel(level))))

    // compose ap
    for (entry <- entries) {
      val assignment = L3_Assignment(L3_FieldAccess(ap(entry)), Duplicate(entry.getEq(level).lhs))

      object L3_ReplaceAccesses extends QuietDefaultStrategy("Local replace of field accesses with temporary fields") {
        this += new Transformation("Search and replace", {
          case access @ L3_FieldAccess(field, _) if entries.exists(field == _.getSolField(level)) =>
            access.target = p(entries.find(field == _.getSolField(level)).get)
            access
        })
      }

      L3_ReplaceAccesses.applyStandalone(assignment)

      stmts += assignment
    }

    for (entry <- entries) {
      val assignment = L3_Assignment(L3_FieldAccess(ar(entry)), Duplicate(entry.getEq(level).lhs))

      object L3_ReplaceAccesses extends QuietDefaultStrategy("Local replace of field accesses with temporary fields") {
        this += new Transformation("Search and replace", {
          case access @ L3_FieldAccess(field, _) if entries.exists(field == _.getSolField(level)) =>
            access.target = entries.find(field == _.getSolField(level)).get.resPerLevel(level)
            access
        })
      }

      L3_ReplaceAccesses.applyStandalone(assignment)

      stmts += assignment
    }

    // main loop
    def cgsSteps = L3_PlainVariableAccess("gen_cgsSteps", L3_IntegerDatatype, false)
    def maxSteps = 512

    stmts += L3_VariableDeclaration(cgsSteps, 0)

    val loopStmts = ListBuffer[L3_Statement]()

    // alpha calculation
    def alphaNom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_alphaNom_${ entry.getSolField(level).name }", L3_RealDatatype, false)
    def alphaDenom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_alphaDenom_${ entry.getSolField(level).name }", L3_RealDatatype, false)
    def alpha = L3_PlainVariableAccess("gen_alpha", L3_RealDatatype, false)

    entries.foreach(entry => {
      loopStmts += L3_VariableDeclaration(alphaNom(entry), L3_FieldAccess(entry.resPerLevel(level)) * L3_FieldAccess(ar(entry)))
      loopStmts += L3_VariableDeclaration(alphaDenom(entry), L3_FieldAccess(ap(entry)) * L3_FieldAccess(ap(entry)))
    })

    loopStmts += L3_VariableDeclaration(alpha,
      entries.map(alphaNom(_) : L3_Expression).reduce(_ + _) / entries.map(alphaDenom(_) : L3_Expression).reduce(_ + _))

    entries.foreach(entry => {
      loopStmts += L3_Assignment(L3_FieldAccess(entry.getSolField(level)), alpha * L3_FieldAccess(p(entry)), "+=", None)
      loopStmts += L3_Assignment(L3_FieldAccess(entry.resPerLevel(level)), alpha * L3_FieldAccess(ap(entry)), "-=", None)
    })

    loopStmts += L3_VariableDeclaration(nextRes, callResNorm)

    // exit criterion
    loopStmts += L3_IfCondition(L3_LowerEqual(nextRes, 0.001 * initRes), ListBuffer[L3_Statement](L3_Return(None)), ListBuffer())

    for (entry <- entries) {
      val assignment = L3_Assignment(L3_FieldAccess(ar(entry)), Duplicate(entry.getEq(level).lhs))

      object L3_ReplaceAccesses extends QuietDefaultStrategy("Local replace of field accesses with temporary fields") {
        this += new Transformation("Search and replace", {
          case access @ L3_FieldAccess(field, _) if entries.exists(field == _.getSolField(level)) =>
            access.target = entries.find(field == _.getSolField(level)).get.resPerLevel(level)
            access
        })
      }

      L3_ReplaceAccesses.applyStandalone(assignment)

      loopStmts += assignment
    }

    def betaNom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_betaNom_${ entry.getSolField(level).name }", L3_RealDatatype, false)
    def betaDenom(entry : L3_SolverForEqEntry) =
      L3_PlainVariableAccess(s"gen_betaDenom_${ entry.getSolField(level).name }", L3_RealDatatype, false)
    def beta = L3_PlainVariableAccess("gen_beta", L3_RealDatatype, false)

    entries.foreach(entry => {
      loopStmts += L3_VariableDeclaration(betaNom(entry), L3_FieldAccess(entry.resPerLevel(level)) * L3_FieldAccess(ar(entry)))
      loopStmts += L3_VariableDeclaration(betaDenom(entry), alphaNom(entry))
    })

    loopStmts += L3_VariableDeclaration(beta,
      entries.map(betaNom(_) : L3_Expression).reduce(_ + _) / entries.map(betaDenom(_) : L3_Expression).reduce(_ + _))

    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(p(entry)), L3_FieldAccess(entry.resPerLevel(level)) + beta * L3_FieldAccess(p(entry))))
    entries.foreach(entry => loopStmts += L3_Assignment(L3_FieldAccess(ap(entry)), L3_FieldAccess(ar(entry)) + beta * L3_FieldAccess(ap(entry))))

    loopStmts += L3_Assignment(curRes, nextRes)

    stmts += L3_ForLoop(maxSteps, Some(cgsSteps), loopStmts)

    stmts += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype),
      ListBuffer[L3_Expression](L3_StringConstant("Maximum number of cgs iterations ("), maxSteps, L3_StringConstant(") was exceeded")))

    stmts
  }
}