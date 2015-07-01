package exastencils.optimization

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.logger._
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.util._

object RemoveDupSIMDLoads extends CustomStrategy("Remove duplicate SIMD loads") {

  private[optimization] final val NEW_DECLS_COND_ANNOT = "RDSL_Decls_Cond"
  private[optimization] final val REPL_ANNOT = "RDSL_Repl"

  override def apply() : Unit = {
    this.transaction()

    Logger.info("Applying strategy " + name)
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    val annotate = new Analyze()
    this.register(annotate)
    this.execute(new Transformation("analyze", PartialFunction.empty))
    this.unregister(annotate)

    this.execute(new Transformation("adapt", Adapt))

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
  }
}

private[optimization] final class Analyze extends Collector {
  import RemoveDupSIMDLoads._

  private var preLoopDecls : ListBuffer[Statement] = null
  private var loads : HashMap[(Expression, HashMap[Expression, Long]), VariableDeclarationStatement] = null
  private var upLoopVar : UpdateLoopVar = null
  private var hasOMPPragma : Boolean = false

  override def enter(node : Node) : Unit = {
    node match {
      case ForLoopStatement(VariableDeclarationStatement(IntegerDatatype, lVar, Some(start)),
        LowerExpression(VariableAccess(lVar3, _), end),
        AssignmentStatement(VariableAccess(lVar2, _), IntegerConstant(incr), "+="),
        _, _) if (lVar == lVar2 && lVar2 == lVar3) =>
        if (node.removeAnnotation(Vectorization.VECT_ANNOT).isDefined) {
          preLoopDecls = new ListBuffer[Statement]
          val newCond = LowerExpression(Duplicate(start), Duplicate(end))
          node.annotate(NEW_DECLS_COND_ANNOT, (preLoopDecls, newCond))
          loads = new HashMap[(Expression, HashMap[Expression, Long]), VariableDeclarationStatement]
          upLoopVar = new UpdateLoopVar(lVar, incr, start)
          hasOMPPragma = node.isInstanceOf[OMP_PotentiallyParallel]
        }

      case decl @ VariableDeclarationStatement(SIMD_RealDatatype, vecTmp,
        Some(load @ SIMD_LoadExpression(UnaryExpression(UnaryOperators.AddressOf, ArrayAccess(base, index, _)), aligned))) =>

        val indSum : HashMap[Expression, Long] = SimplifyExpression.extractIntegralSum(index)
        val other = loads.get((base, indSum))

        if (other.isDefined)
          decl.expression = Some(new VariableAccess(other.get.name, SIMD_RealDatatype))

        else {
          loads((base, indSum)) = decl

          // test if the vector can be reused next iteration
          if (!hasOMPPragma) {
            val indSumNIt : HashMap[Expression, Long] = SimplifyExpression.extractIntegralSum(upLoopVar.updateDup(index))
            val nextIt = loads.get((base, indSumNIt))
            if (nextIt.isDefined) {
              preLoopDecls += new VariableDeclarationStatement(SIMD_RealDatatype, vecTmp,
                SIMD_LoadExpression(UnaryExpression(UnaryOperators.AddressOf,
                  ArrayAccess(Duplicate(base), SimplifyExpression.simplifyIntegralExpr(upLoopVar.replaceDup(index)))), aligned))
              decl.annotate(REPL_ANNOT, AssignmentStatement(new VariableAccess(vecTmp, SIMD_RealDatatype), load, "="))
              if (nextIt.get.hasAnnotation(REPL_ANNOT))
                nextIt.get.annotate(REPL_ANNOT, AssignmentStatement(new VariableAccess(nextIt.get.name, SIMD_RealDatatype),
                  new VariableAccess(vecTmp, SIMD_RealDatatype), "=")) // TODO: check if this is always correct...
              else
                nextIt.get.annotate(REPL_ANNOT, new VariableDeclarationStatement(SIMD_RealDatatype, nextIt.get.name,
                  new VariableAccess(vecTmp, SIMD_RealDatatype)))
            }
          }
        }

      case _ => /* nothing to do */
    }
  }

  override def leave(node : Node) : Unit = {
    if (node.hasAnnotation(NEW_DECLS_COND_ANNOT)) {
      preLoopDecls = null
      loads = null
      upLoopVar = null
    }
  }

  override def reset() : Unit = {
    preLoopDecls = null
  }

  private class UpdateLoopVar(itName : String, offset : Long, nju : Expression)
      extends QuietDefaultStrategy("Add loop var offset") {

    private final val SKIP_ANNOT = "RDSL_Skip"
    private var replace : Boolean = false

    this += new Transformation("apply", {
      case vAcc @ VariableAccess(v, Some(IntegerDatatype)) if (v == itName) =>
        if (replace)
          SubtractionExpression(Duplicate(nju), IntegerConstant(offset))
        else if (!vAcc.removeAnnotation(SKIP_ANNOT).isDefined) {
          vAcc.annotate(SKIP_ANNOT) // already done
          SubtractionExpression(vAcc, IntegerConstant(offset))
        } else
          vAcc
    })

    def updateDup(expr : Expression) : Expression = {
      val expr2 = FreeStatement(Duplicate(expr)) // just a temporary wrapper...
      replace = false
      applyStandalone(expr2)
      return expr2.pointer
    }

    def replaceDup(expr : Expression) : Expression = {
      val expr2 = FreeStatement(Duplicate(expr)) // just a temporary wrapper...
      replace = true
      applyStandalone(expr2)
      return expr2.pointer
    }
  }
}

private final object Adapt extends PartialFunction[Node, Transformation.OutputType] {
  import RemoveDupSIMDLoads._

  def isDefinedAt(node : Node) : Boolean = {
    return node.hasAnnotation(NEW_DECLS_COND_ANNOT) || node.hasAnnotation(REPL_ANNOT)
  }

  def apply(node : Node) : Transformation.OutputType = {

    val decls = node.removeAnnotation(NEW_DECLS_COND_ANNOT)
    if (decls.isDefined) {
      val (stmts, cond) = decls.get.value.asInstanceOf[(ListBuffer[Statement], Expression)]
      stmts += node.asInstanceOf[Statement]
      return new ConditionStatement(cond, stmts)
    }

    return node.removeAnnotation(REPL_ANNOT).get.value.asInstanceOf[Node]
  }
}
