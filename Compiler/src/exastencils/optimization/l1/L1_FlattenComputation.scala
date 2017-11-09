package exastencils.optimization.l1

import scala.collection.mutable._

import exastencils.base.l1.L1_ImplicitConversion._
import exastencils.base.l1._
import exastencils.core.Duplicate
import exastencils.datastructures._

/// L1_FlattenComputation

object L1_FlattenComputation extends QuietDefaultStrategy("Flatten complex computation expressions") {
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
    case sub : L1_Subtraction =>
      changed = true
      val add = L1_Addition(sub.left)
      add.summands += L1_Negative(sub.right)
      add
  })

  this += new Transformation("Resolve additions nested in multiplications", {
    // (a + b) * (c + d) => a*c + a*d + b*c + b*d
    case mult : L1_Multiplication =>
      var nonAdditions = mult.factors
      var additions = ListBuffer[L1_Addition]()
      for (ex <- mult.factors) ex match {
        case add : L1_Addition =>
          nonAdditions -= add
          additions += add
        case _                 =>
      }

      if (additions.nonEmpty) {
        changed = true
        val ret = L1_Addition()
        for (add <- additions) {
          if (ret.summands.isEmpty)
            ret.summands = add.summands.map(s => L1_Multiplication(s +: Duplicate(nonAdditions)))
          else
            ret.summands = add.summands.flatMap(s => ret.summands.map(r => L1_Multiplication(Duplicate(s), Duplicate(r))))
        }

        ret
      } else
        mult
  })

  this += new Transformation("Split divisions with additions and multiplications in the numerator", {
    // (a + b) / c => a/c + b/c
    case L1_Division(add : L1_Addition, div) =>
      changed = true
      L1_Addition(add.summands.map(ex => L1_Division(ex, Duplicate(div)) : L1_Expression))

    // (a * b) / c => a*c + b*c
    case L1_Division(mult : L1_Multiplication, div) =>
      changed = true
      mult.factors += L1_Division(1.0, Duplicate(div))
      mult
  })

  this += new Transformation("Flatten nested multiplications", {
    // a * (b * c) => a * b * c
    case mult : L1_Multiplication =>
      val newSummands = ListBuffer[L1_Expression]()
      var localChange = false
      var negative = false
      for (ex <- mult.factors) ex match {
        case mult : L1_Multiplication =>
          localChange = true
          newSummands ++= mult.factors.map {
            case L1_Negative(ex2) =>
              negative = !negative
              ex2
            case ex2              => ex2
          }

        case L1_Negative(ex2) =>
          localChange = true
          negative = !negative
          newSummands += ex2

        case _ =>
          newSummands += ex
      }

      if (localChange) {
        changed = true
        if (negative)
          L1_Negative(L1_Multiplication(newSummands))
        else
          L1_Multiplication(newSummands)
      } else
        mult
  })

  this += new Transformation("Flatten nested additions", {
    // a + (b + c) => a + b + c
    case add : L1_Addition =>
      val newSummands = ListBuffer[L1_Expression]()
      var localChange = false
      for (ex <- add.summands) ex match {
        case add : L1_Addition =>
          localChange = true
          newSummands ++= add.summands

        case _ => newSummands += ex
      }

      if (localChange) {
        changed = true
        L1_Addition(newSummands)
      } else
        add
  })

  this += new Transformation("Resolve double negatives", {
    // -(-a) => a
    case L1_Negative(L1_Negative(ex)) =>
      changed = true
      ex
  })
}
