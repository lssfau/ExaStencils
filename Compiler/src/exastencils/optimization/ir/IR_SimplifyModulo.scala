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
  val collector = new IR_ModuloContextCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  private val REPLACE_ANNOT = "SimplModRepl"

  this += new Transformation("now", {
    case m @ IR_Modulo(num, IR_IntegerConstant(den)) =>
      try {
        val sum : HashMap[IR_Expression, Long] = IR_SimplifyExpression.extractIntegralSum(num)
        for ((exp, coeff) <- sum) {
          exp match {
            case va @ IR_VariableAccess(n, IR_IntegerDatatype)   =>
              for ((_, _, incr, init) <- collector.loopStrides.find(_._2 == n))
                if ((coeff * incr) % den == 0)
                  va.annotate(REPLACE_ANNOT, init)
            case IR_Division(divNum, IR_IntegerConstant(divDen)) =>
              val divSum : HashMap[IR_Expression, Long] = IR_SimplifyExpression.extractIntegralSum(divNum)
              for ((divExp, divCoeff) <- divSum) {
                divExp match {
                  case va @ IR_VariableAccess(n, IR_IntegerDatatype) =>
                    for ((_, _, incr, init) <- collector.loopStrides.find(_._2 == n))
                      if ((divCoeff * coeff * incr) % (den * divDen) == 0)
                        va.annotate(REPLACE_ANNOT, init)
                  case _                                             =>
                }
              }
            case _                                               =>
          }
        }
        IR_Modulo(IR_SimplifyExpression.recreateExprFromIntSum(sum), IR_IntegerConstant(den))
      } catch {
        case _ : EvaluationException =>
          m
      }

    case n if (n.hasAnnotation(REPLACE_ANNOT)) =>
      Duplicate(n.removeAnnotation(REPLACE_ANNOT).get).asInstanceOf[IR_Expression]
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
