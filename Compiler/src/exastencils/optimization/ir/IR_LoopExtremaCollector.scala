package exastencils.optimization.ir

import scala.collection.mutable

import exastencils.base.ir._
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node

class IR_LoopExtremaCollector extends Collector {
  val extremaMap = new mutable.HashMap[String, (Long, Long)]()

  override def enter(node : Node) : Unit = {
    node match {
      case IR_ForLoop(IR_VariableDeclaration(_, lVar, Some(init), _), end, _, _, _) =>
        try {
          val initExtrema : (Long, Long) = IR_SimplifyExpression.evalIntegralExtrema(init, extremaMap)
          end match {
            case IR_Lower(IR_VariableAccess(n, _), max) if (n == lVar)        =>
              extremaMap(lVar) = (initExtrema._1, IR_SimplifyExpression.evalIntegralExtrema(max, extremaMap)._2 - 1)
            case IR_LowerEqual(IR_VariableAccess(n, _), max) if (n == lVar)   =>
              extremaMap(lVar) = (initExtrema._1, IR_SimplifyExpression.evalIntegralExtrema(max, extremaMap)._2)
            case IR_Greater(IR_VariableAccess(n, _), min) if (n == lVar)      =>
              extremaMap(lVar) = (IR_SimplifyExpression.evalIntegralExtrema(min, extremaMap)._1 + 1, initExtrema._2)
            case IR_GreaterEqual(IR_VariableAccess(n, _), min) if (n == lVar) =>
              extremaMap(lVar) = (IR_SimplifyExpression.evalIntegralExtrema(min, extremaMap)._1, initExtrema._2)
            case _                                                            =>
          }
        } catch {
          case _ : EvaluationException =>
        }
      case _                                                                        =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case IR_ForLoop(IR_VariableDeclaration(_, lVar, _, _), _, _, _, _) =>
        extremaMap.remove(lVar)
      case _                                                             =>
    }
  }

  override def reset() : Unit = {
    extremaMap.clear()
  }
}
