package exastencils.solver.l3

import scala.collection.mutable._

import exastencils.base.ExaRootNode
import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.l3._
import exastencils.logger.Logger
import exastencils.util.l3.L3_MathFunctionReference

object L3_IterativeSolverForEquation {
  def generateDebugPrints : Boolean = Knowledge.solver_generateDbgOutputs

  def generateIterativeSolver(solver : String, entries : ListBuffer[L3_SolverForEqEntry], level : Int, smootherHints : ListBuffer[L3_GenerateSmootherHint]) = {
    solver.toLowerCase() match {
      case "cg" | "conjugategradient" | "conjugategradients" => L3_ConjugateGradientForEquation.generateFor(entries, level)
      case "bicgstab"                                        => L3_BiCGStabForEquation.generateFor(entries, level)
      case "minres"                                          => L3_MinResForEquation.generateFor(entries, level)
      case "cr" | "conjugateresidual" | "conjugateresiduals" => L3_ConjugateResidualForEquation.generateFor(entries, level)
      case "smoother"                                        => L3_VankaAsSolverForEquation.generateFor(entries, level, smootherHints)
      case _                                                 => Logger.error(s"Unsupported iterative solver: ${ solver }")
    }
  }

  val resNormFctDoneForLevels = HashSet[Int]()

  def generateResNormFunction(entries : ListBuffer[L3_SolverForEqEntry], level : Int) = {
    if (!resNormFctDoneForLevels.contains(level)) { // only generate once per level
      resNormFctDoneForLevels += level

      val fctBody = ListBuffer[L3_Statement]()

      def resNorm = L3_PlainVariableAccess("gen_resNorm", L3_RealDatatype, false)

      fctBody += L3_VariableDeclaration(resNorm, 0.0)

      entries.foreach(entry => {
        //fctBody += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype), L3_FieldFieldConvolution(L3_FieldAccess(entry.resPerLevel(level)), L3_FieldAccess(entry.resPerLevel(level))))
        fctBody += L3_Assignment(resNorm,
          L3_FieldFieldConvolution(L3_FieldAccess(entry.resPerLevel(level)), L3_FieldAccess(entry.resPerLevel(level))),
          "+=", None)
      })

      fctBody += L3_Return(Some(L3_FunctionCall(L3_MathFunctionReference("sqrt", L3_RealDatatype), resNorm)))

      val fct = L3_LeveledFunction(s"gen_resNorm", level, L3_RealDatatype, ListBuffer(), fctBody)
      ExaRootNode.l3_root.nodes += fct
    }
  }

  def generateOperatorApplication(dest : L3_Field, srcExpr : L3_Expression, srcMap : HashMap[L3_SolverForEqEntry, L3_Field],
      entries : ListBuffer[L3_SolverForEqEntry], entry : L3_SolverForEqEntry, level : Int) = {
    val assignment = L3_Assignment(L3_FieldAccess(dest), Duplicate(srcExpr))

    object L3_ReplaceAccesses extends QuietDefaultStrategy("Local replace of field accesses with temporary fields") {
      this += new Transformation("Search and replace", {
        case access @ L3_FieldAccess(field, _, _) if entries.exists(field == _.getSolField(level)) =>
          access.target = srcMap(entries.find(field == _.getSolField(level)).get)
          access
      })
    }

    L3_ReplaceAccesses.applyStandalone(assignment)

    assignment
  }
}

trait L3_IterativeSolverForEquation {
  val logCharacteristics = false
  def generateFor(entries : ListBuffer[L3_SolverForEqEntry], level : Int) : ListBuffer[L3_Statement]
}
