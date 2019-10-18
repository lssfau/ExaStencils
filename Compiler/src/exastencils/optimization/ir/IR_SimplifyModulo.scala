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

package exastencils.optimization.ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.core.collectors.Collector
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.logger.Logger

object IR_SimplifyModulo extends DefaultStrategy("simplify modulo expressions") {
  val modCtxCollector = new IR_ModuloContextCollector
  val loopExtremaCollector = new IR_LoopExtremaCollector()
  this.register(modCtxCollector)
  this.register(loopExtremaCollector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("now", {
    case m @ IR_Modulo(num, IR_IntegerConstant(den)) =>
      try {
        val (omin, omax) =  IR_SimplifyExpression.evalIntegralExtrema(num, loopExtremaCollector.extremaMap)
        val sum : HashMap[IR_Expression, Long] = IR_SimplifyExpression.extractIntegralSum(num)
        val update = ListBuffer[() => Unit]() // prevent modifications of sum during its traversal
        for ((exp, coeff) <- sum) {
          exp match {
            case IR_VariableAccess(n, IR_IntegerDatatype)   =>
              for ((_, _, incr, init) <- modCtxCollector.loopStrides.find(_._2 == n))
                if ((coeff * incr) % den == 0) {
                  val dupInit = Duplicate(init)
                  update += { () =>
                    sum(exp) = 0
                    sum(dupInit) = sum.getOrElse(dupInit, 0L) + 1
                  }
                }
            case IR_Division(divNum, IR_IntegerConstant(divDen)) =>
              val divSum : HashMap[IR_Expression, Long] = IR_SimplifyExpression.extractIntegralSum(divNum)
              val updateInner = ListBuffer[() => Unit]() // prevent modifications of divSum during its traversal
              for ((divExp, divCoeff) <- divSum) {
                divExp match {
                  case IR_VariableAccess(n, IR_IntegerDatatype) =>
                    for ((_, _, incr, init) <- modCtxCollector.loopStrides.find(_._2 == n))
                      if ((divCoeff * coeff * incr) % (den * divDen) == 0) {
                        val dupInit = Duplicate(init)
                        updateInner += { () =>
                          divSum(divExp) = 0
                          divSum(dupInit) = divSum.getOrElse(dupInit, 0L) + 1
                        }
                      }
                  case _                                             =>
                }
              }
              if (!updateInner.isEmpty) { // incorporate changes stored in updateInner
                for (u <- updateInner)
                  u()
                val newExp = IR_SimplifyExpression.recreateExprFromIntSum(divSum)
                update += { () =>
                  sum(exp) = 0
                  sum(newExp) = sum.getOrElse(newExp, 0L) + 1
                }
              }
            case _                                               =>
          }
        }
        if (!update.isEmpty) { // incorporate changes stored in update
          for (u <- update)
            u()
          val newNum = IR_SimplifyExpression.recreateExprFromIntSum(sum)
          val (nmin, nmax) =  IR_SimplifyExpression.evalIntegralExtrema(newNum, loopExtremaCollector.extremaMap)
          if (math.min(omin, nmin) >= 0 || math.max(omax, nmax) <= 0)
            IR_Modulo(newNum, IR_IntegerConstant(den)) // we made some modifications and the sign will not change: modifications preserve semantics
          else
            m // whoops, sign changed -> semantics changed, use old
        } else
          m // no change at all, use old
      } catch {
        case _ : EvaluationException =>
          m
      }
  })
}

class IR_ModuloContextCollector extends Collector {

  var loopStrides : List[(IR_ForLoop, String, Long, IR_Expression)] = Nil

  override def enter(node : Node) : Unit = {
    node match {
      case l : IR_ForLoop =>
        l.begin match {
          case IR_VariableDeclaration(IR_IntegerDatatype, initN, Some(init), _) =>
            l.inc match {
              case IR_Assignment(IR_VariableAccess(iter, IR_IntegerDatatype), IR_IntegerConstant(inc),
              "+=") if (initN == iter) =>
                loopStrides ::= (l, iter, inc, init)

              case IR_Assignment(IR_VariableAccess(iter, IR_IntegerDatatype), IR_Addition(ListBuffer(
              IR_VariableAccess(iter2, IR_IntegerDatatype), IR_IntegerConstant(inc))),
              "=") if (initN == iter && iter == iter2) =>
                loopStrides ::= (l, iter, inc, init)

              case IR_Assignment(IR_VariableAccess(iter, IR_IntegerDatatype), IR_Addition(ListBuffer(
              IR_IntegerConstant(inc), IR_VariableAccess(iter2, IR_IntegerDatatype))),
              "=") if (initN == iter && iter == iter2) =>
                loopStrides ::= (l, iter, inc, init)

              case _ =>
                loopStrides ::= (l, null, 0, null)
            }
          case _                                                                =>
            loopStrides ::= (l, null, 0, null)
        }

      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case l : IR_ForLoop =>
        if (loopStrides.head._1 ne l)
          Logger.error(s"${ this.getClass().getSimpleName() } mismatch, stack head does not match node given in leave call: $l")
        loopStrides = loopStrides.tail

      case _ =>
    }
  }

  override def reset() : Unit = {
    loopStrides = Nil
  }
}
