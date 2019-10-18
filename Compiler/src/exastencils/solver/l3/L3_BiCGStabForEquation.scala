//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.solver.l3

import scala.collection.mutable._

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_ForLoop
import exastencils.boundary.l3.L3_NoBC
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.l3._

/// L3_BiCGStabForEquation

object L3_BiCGStabForEquation extends L3_IterativeSolverForEquation {

  import L3_IterativeSolverForEquation._

  override def generateFor(entries : ListBuffer[L3_SolverForEqEntry], level : Int) = {
    // set up function for norm of residual
    generateResNormFunction(entries, level)
    // set up temporary fields
    val v = HashMap[L3_SolverForEqEntry, L3_Field]()
    val p = HashMap[L3_SolverForEqEntry, L3_Field]()
    val s = HashMap[L3_SolverForEqEntry, L3_Field]()
    val t = HashMap[L3_SolverForEqEntry, L3_Field]()
    val resHat = HashMap[L3_SolverForEqEntry, L3_Field]()

    for (entry <- entries) {
      val sol = entry.getSolField(level)
      val res = entry.resPerLevel(level)

      val vField = L3_Field(s"gen_v_${ sol.name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), L3_NoBC)
      val pField = L3_Field(s"gen_p_${ sol.name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))
      val sField = L3_Field(s"gen_s_${ sol.name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), Duplicate(res.boundary))
      val tField = L3_Field(s"gen_t_${ sol.name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), L3_NoBC)
      val resHatField = L3_Field(s"gen_resHat_${ sol.name }", level, res.domain,
        res.datatype, res.localization, Some(0.0), L3_NoBC)

      L3_FieldCollection.add(vField)
      L3_FieldCollection.add(pField)
      L3_FieldCollection.add(sField)
      L3_FieldCollection.add(tField)
      L3_FieldCollection.add(resHatField)

      v += (entry -> vField)
      p += (entry -> pField)
      s += (entry -> sField)
      t += (entry -> tField)
      resHat += (entry -> resHatField)
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

    entries.foreach(entry => stmts += L3_Assignment(L3_FieldAccess(resHat(entry)), L3_FieldAccess(entry.resPerLevel(level))))

    def rho = L3_PlainVariableAccess("gen_rho", L3_RealDatatype, false)
    def rhoOld = L3_PlainVariableAccess("gen_rhoOld", L3_RealDatatype, false)
    def alpha = L3_PlainVariableAccess("gen_alpha", L3_RealDatatype, false)
    def beta = L3_PlainVariableAccess("gen_beta", L3_RealDatatype, false)
    def omega = L3_PlainVariableAccess("gen_omega", L3_RealDatatype, false)

    stmts += L3_VariableDeclaration(rho, 1.0)
    stmts += L3_VariableDeclaration(rhoOld, 1.0)
    stmts += L3_VariableDeclaration(alpha, 1.0)
    stmts += L3_VariableDeclaration(beta, 1.0)
    stmts += L3_VariableDeclaration(omega, 1.0)

    entries.foreach(entry => stmts += L3_Assignment(L3_FieldAccess(v(entry)), 0.0))
    entries.foreach(entry => stmts += L3_Assignment(L3_FieldAccess(p(entry)), 0.0))

    // main loop
    def curStep = L3_PlainVariableAccess("gen_curStep", L3_IntegerDatatype, false)

    stmts += L3_VariableDeclaration(curStep, 0)

    val loopStmts = ListBuffer[L3_Statement]()

    if (Knowledge.solver_cgs_restart) {
      loopStmts += L3_IfCondition((curStep > 0) AndAnd (0 EqEq (curStep Mod Knowledge.solver_cgs_restartAfter)),
        entries.map(_.generateUpdateRes(level)) ++
          entries.map(entry => L3_Assignment(L3_FieldAccess(resHat(entry)), L3_FieldAccess(entry.resPerLevel(level)))) ++
          ListBuffer[L3_Statement](
            L3_Assignment(rho, 1.0),
            L3_Assignment(rhoOld, 1.0),
            L3_Assignment(alpha, 1.0),
            L3_Assignment(beta, 1.0),
            L3_Assignment(omega, 1.0)) ++
          entries.map(entry => L3_Assignment(L3_FieldAccess(v(entry)), 0.0) : L3_Statement) ++
          entries.map(entry => L3_Assignment(L3_FieldAccess(p(entry)), 0.0) : L3_Statement)
      )
    }

    loopStmts += L3_Assignment(rhoOld, rho)
    loopStmts += L3_Assignment(rho, 0.0)
    entries.foreach(entry => loopStmts += L3_Assignment(rho,
      L3_FieldFieldConvolution(L3_FieldAccess(resHat(entry)), L3_FieldAccess(entry.resPerLevel(level))), "+=", None))

    loopStmts += L3_Assignment(beta, (rho / rhoOld) * (alpha / omega))
    entries.foreach(entry =>
      loopStmts += L3_Assignment(L3_FieldAccess(p(entry)),
        L3_FieldAccess(entry.resPerLevel(level)) + beta * (L3_FieldAccess(p(entry)) - omega * L3_FieldAccess(v(entry)))))

    entries.foreach(entry =>
      loopStmts += generateOperatorApplication(v(entry), entry.getEq(level).lhs, p, entries, entry, level))

    def alphaDenom = L3_PlainVariableAccess("gen_alphaDenom", L3_RealDatatype, false)
    loopStmts += L3_VariableDeclaration(alphaDenom, 0.0)
    entries.foreach(entry => loopStmts += L3_Assignment(alphaDenom,
      L3_FieldFieldConvolution(L3_FieldAccess(resHat(entry)), L3_FieldAccess(v(entry))), "+=", None))
    loopStmts += L3_Assignment(alpha, rho / alphaDenom)

    entries.foreach(entry =>
      loopStmts += L3_Assignment(L3_FieldAccess(s(entry)), L3_FieldAccess(entry.resPerLevel(level)) - alpha * L3_FieldAccess(v(entry))))

    entries.foreach(entry =>
      loopStmts += generateOperatorApplication(t(entry), entry.getEq(level).lhs, s, entries, entry, level))

    def omegaNom = L3_PlainVariableAccess("gen_omegaNom", L3_RealDatatype, false)
    def omegaDenom = L3_PlainVariableAccess("gen_omegaDenom", L3_RealDatatype, false)
    loopStmts += L3_VariableDeclaration(omegaNom, 0.0)
    loopStmts += L3_VariableDeclaration(omegaDenom, 0.0)

    entries.foreach(entry => {
      loopStmts += L3_Assignment(omegaNom,
        L3_FieldFieldConvolution(L3_FieldAccess(t(entry)), L3_FieldAccess(s(entry))), "+=", None)
      loopStmts += L3_Assignment(omegaDenom,
        L3_FieldFieldConvolution(L3_FieldAccess(t(entry)), L3_FieldAccess(t(entry))), "+=", None)
    })

    loopStmts += L3_Assignment(omega, omegaNom / omegaDenom)

    entries.foreach(entry =>
      loopStmts += L3_Assignment(L3_FieldAccess(entry.getSolField(level)), L3_FieldAccess(entry.getSolField(level)) + alpha * L3_FieldAccess(p(entry)) + omega * L3_FieldAccess(s(entry))))

    entries.foreach(entry =>
      loopStmts += L3_Assignment(L3_FieldAccess(entry.resPerLevel(level)), L3_FieldAccess(s(entry)) - omega * L3_FieldAccess(t(entry))))

    // exit criterion
    loopStmts += L3_Assignment(curRes, callResNorm)

    val returnStmts = ListBuffer[L3_Statement]()
    if (L3_IterativeSolverForEquation.generateDebugPrints) {
      returnStmts += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype), ListBuffer[L3_Expression](
        L3_StringConstant("BiCGStab took"), curStep, L3_StringConstant("steps to reduce residual from"), initRes, L3_StringConstant("to"), curRes))
    }
    if (logCharacteristics)
      returnStmts += L3_FunctionCall(L3_PlainInternalFunctionReference("logCharacteristics", L3_UnitDatatype), L3_StringConstant("cgs_num_it"), curStep)
    returnStmts += L3_Return(None)

    loopStmts += L3_IfCondition((curRes <= Knowledge.solver_cgs_targetResReduction * initRes)
      OrOr (curRes <= Knowledge.solver_cgs_absResThreshold), returnStmts, ListBuffer())

    stmts += L3_ForLoop(Knowledge.solver_cgs_maxNumIts, Some(curStep), loopStmts)

    stmts += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype),
      ListBuffer[L3_Expression](L3_StringConstant("Maximum number of cgs iterations ("), Knowledge.solver_cgs_maxNumIts, L3_StringConstant(") was exceeded")))
    if (logCharacteristics)
      stmts += L3_FunctionCall(L3_PlainInternalFunctionReference("logCharacteristics", L3_UnitDatatype), L3_StringConstant("cgs_num_it"), curStep)

    stmts
  }
}
