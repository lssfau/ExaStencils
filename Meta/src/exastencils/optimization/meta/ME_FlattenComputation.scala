package exastencils.optimization.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_FlattenComputation extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/optimization/|LAYER_LC|/|LAYER_UC|_FlattenComputation.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.optimization.|LAYER_LC|

import scala.collection.mutable._

import exastencils.base.|LAYER_LC|.|LAYER_UC|_ImplicitConversion._
import exastencils.base.|LAYER_LC|._
import exastencils.core.Duplicate
import exastencils.datastructures._

/// |LAYER_UC|_FlattenComputation

object |LAYER_UC|_FlattenComputation extends QuietDefaultStrategy("Flatten complex computation expressions") {
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
    case sub : |LAYER_UC|_Subtraction =>
      changed = true
      val add = |LAYER_UC|_Addition(sub.left)
      add.summands += |LAYER_UC|_Negative(sub.right)
      add
  })

  this += new Transformation("Resolve additions nested in multiplications", {
    // (a + b) * (c + d) => a*c + a*d + b*c + b*d
    case mult : |LAYER_UC|_Multiplication =>
      var nonAdditions = mult.factors
      var additions = ListBuffer[|LAYER_UC|_Addition]()
      for (ex <- mult.factors) ex match {
        case add : |LAYER_UC|_Addition =>
          nonAdditions -= add
          additions += add
        case _                 =>
      }

      if (additions.nonEmpty) {
        changed = true
        val ret = |LAYER_UC|_Addition()
        for (add <- additions) {
          if (ret.summands.isEmpty)
            ret.summands = add.summands.map(s => |LAYER_UC|_Multiplication(s +: Duplicate(nonAdditions)))
          else
            ret.summands = add.summands.flatMap(s => ret.summands.map(r => |LAYER_UC|_Multiplication(Duplicate(s), Duplicate(r))))
        }

        ret
      } else
        mult
  })

  this += new Transformation("Split divisions with additions and multiplications in the numerator", {
    // (a + b) / c => a/c + b/c
    case |LAYER_UC|_Division(add : |LAYER_UC|_Addition, div) =>
      changed = true
      |LAYER_UC|_Addition(add.summands.map(ex => |LAYER_UC|_Division(ex, Duplicate(div)) : |LAYER_UC|_Expression))

    // (a * b) / c => a*c + b*c
    case |LAYER_UC|_Division(mult : |LAYER_UC|_Multiplication, div) =>
      changed = true
      mult.factors += |LAYER_UC|_Division(1.0, Duplicate(div))
      mult
  })

  this += new Transformation("Flatten nested multiplications", {
    // a * (b * c) => a * b * c
    case mult : |LAYER_UC|_Multiplication =>
      val newSummands = ListBuffer[|LAYER_UC|_Expression]()
      var localChange = false
      var negative = false
      for (ex <- mult.factors) ex match {
        case mult : |LAYER_UC|_Multiplication =>
          localChange = true
          newSummands ++= mult.factors.map {
            case |LAYER_UC|_Negative(ex2) =>
              negative = !negative
              ex2
            case ex2              => ex2
          }

        case |LAYER_UC|_Negative(ex2) =>
          localChange = true
          negative = !negative
          newSummands += ex2

        case _ =>
          newSummands += ex
      }

      if (localChange) {
        changed = true
        if (negative)
          |LAYER_UC|_Negative(|LAYER_UC|_Multiplication(newSummands))
        else
          |LAYER_UC|_Multiplication(newSummands)
      } else
        mult
  })

  this += new Transformation("Flatten nested additions", {
    // a + (b + c) => a + b + c
    case add : |LAYER_UC|_Addition =>
      val newSummands = ListBuffer[|LAYER_UC|_Expression]()
      var localChange = false
      for (ex <- add.summands) ex match {
        case add : |LAYER_UC|_Addition =>
          localChange = true
          newSummands ++= add.summands

        case _ => newSummands += ex
      }

      if (localChange) {
        changed = true
        |LAYER_UC|_Addition(newSummands)
      } else
        add
  })

  this += new Transformation("Resolve double negatives", {
    // -(-a) => a
    case |LAYER_UC|_Negative(|LAYER_UC|_Negative(ex)) =>
      changed = true
      ex
  })
}
"""
  }
}
