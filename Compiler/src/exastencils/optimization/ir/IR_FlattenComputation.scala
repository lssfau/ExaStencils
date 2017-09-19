package exastencils.optimization.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures._

/// IR_FlattenComputation

object IR_FlattenComputation extends QuietDefaultStrategy("Flatten complex computation expressions") {
  var changed : Boolean = false

  def doUntilDone(node : Option[Node] = None) = {
    do {
      changed = false
      apply(node)
    } while (changed)
  }

  def doUntilDoneStandalone(node : Node) = {
    do {
      changed = false
      applyStandalone(node)
    } while (changed)
  }

  def doUntilDoneStandalone[T](nodes : Buffer[T]) = {
    do {
      changed = false
      applyStandalone(nodes)
    } while (changed)
  }

  this += new Transformation("Convert subtractions to additions", {
    case sub : IR_Subtraction =>
      changed = true
      val add = IR_Addition(sub.left)
      add.summands += IR_Negative(sub.right)
      add
  }, isParallel = true)

  this += new Transformation("Resolve additions nested in multiplications", {
    // (a + b) * (c + d) => a*c + a*d + b*c + b*d
    case mult : IR_Multiplication =>
      var nonAdditions = mult.factors
      var additions = ListBuffer[IR_Addition]()
      for (ex <- mult.factors) ex match {
        case add : IR_Addition =>
          nonAdditions -= add
          additions += add
        case _                 =>
      }

      if (additions.nonEmpty) {
        changed = true
        val ret = IR_Addition()
        for (add <- additions) {
          if (ret.summands.isEmpty)
            ret.summands = add.summands.map(s => IR_Multiplication(s +: Duplicate(nonAdditions)))
          else
            ret.summands = add.summands.flatMap(s => ret.summands.map(r => IR_Multiplication(Duplicate(s), Duplicate(r))))
        }

        ret
      } else
        mult
  }, isParallel = true)

  this += new Transformation("Split divisions with additions and multiplications in the numerator", {
    // (a + b) / c => a/c + b/c
    case IR_Division(add : IR_Addition, div) =>
      changed = true
      IR_Addition(add.summands.map(ex => IR_Division(ex, Duplicate(div)) : IR_Expression))

    // (a * b) / c => a * b * (1 / c)
    case IR_Division(mult : IR_Multiplication, div) =>
      changed = true
      IR_Multiplication(mult.factors :+ IR_Division(1.0, div))
  }, isParallel = true)

  this += new Transformation("Flatten nested multiplications", {
    // a * (b * c) => a * b * c
    case mult : IR_Multiplication =>
      val newSummands = ListBuffer[IR_Expression]()
      var localChange = false
      var negative = false
      for (ex <- mult.factors) ex match {
        case mult : IR_Multiplication =>
          localChange = true
          newSummands ++= mult.factors.map {
            case IR_Negative(ex2) =>
              negative = !negative
              ex2
            case ex2              => ex2
          }

        case IR_Negative(ex2) =>
          localChange = true
          negative = !negative
          newSummands += ex2

        case _ =>
          newSummands += ex
      }

      if (localChange) {
        changed = true
        if (negative)
          IR_Negative(IR_Multiplication(newSummands))
        else
          IR_Multiplication(newSummands)
      } else
        mult
  }, isParallel = true)

  this += new Transformation("Flatten nested additions", {
    // a + (b + c) => a + b + c
    case add : IR_Addition =>
      val newSummands = ListBuffer[IR_Expression]()
      var localChange = false
      for (ex <- add.summands) ex match {
        case add : IR_Addition =>
          localChange = true
          newSummands ++= add.summands

        case _ => newSummands += ex
      }

      if (localChange) {
        changed = true
        IR_Addition(newSummands)
      } else
        add
  }, isParallel = true)

  this += new Transformation("Resolve double negatives", {
    // -(-a) => a
    case IR_Negative(IR_Negative(ex)) =>
      changed = true
      ex
  }, isParallel = true)
}
