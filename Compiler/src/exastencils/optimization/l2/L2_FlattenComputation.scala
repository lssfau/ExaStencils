package exastencils.optimization.l2

import scala.collection.mutable._

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.l2.L2_FieldAccess
import exastencils.logger.Logger
import exastencils.operator.l2.L2_StencilAccess

/// L2_FlattenComputation

object L2_FlattenComputation extends QuietDefaultStrategy("Flatten complex computation expressions") {
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
    case sub : L2_Subtraction =>
      changed = true
      val add = L2_Addition(sub.left)
      add.summands += L2_Negative(sub.right)
      add
  })

  this += new Transformation("Resolve additions nested in multiplications", {
    // (a + b) * (c + d) => a*c + a*d + b*c + b*d
    case mult : L2_Multiplication =>
      var nonAdditions = mult.factors
      var additions = ListBuffer[L2_Addition]()
      for (ex <- mult.factors) ex match {
        case add : L2_Addition =>
          nonAdditions -= add
          additions += add
        case _                 =>
      }

      if (additions.nonEmpty) {
        changed = true
        val ret = L2_Addition()
        for (add <- additions) {
          if (ret.summands.isEmpty)
            ret.summands = add.summands.map(s => L2_Multiplication(s +: Duplicate(nonAdditions)))
          else
            ret.summands = add.summands.flatMap(s => ret.summands.map(r => L2_Multiplication(Duplicate(s), Duplicate(r))))
        }

        ret
      } else
        mult
  })

  this += new Transformation("Split divisions with additions and multiplications in the numerator", {
    // (a + b) / c => a/c + b/c
    case L2_Division(add : L2_Addition, div) =>
      changed = true
      L2_Addition(add.summands.map(ex => L2_Division(ex, Duplicate(div)) : L2_Expression))

    // (a * b) / c => a*c + b*c
    case L2_Division(mult : L2_Multiplication, div) =>
      changed = true
      mult.factors += L2_Division(1.0, Duplicate(div))
      mult
  })

  this += new Transformation("Resolve stencil-field convolutions", {
    // sten * field => c:[0,0] * field@[0,0] + ...
    case mult @ L2_Multiplication(factors) =>
      val newFactors = ListBuffer[L2_Expression]()
      var localChange = false

      var skipNext = false
      for (i <- factors.indices) factors(i) match {
        case _ if skipNext => skipNext = false

        case sten : L2_StencilAccess =>
          if (sten.target.colStride.exists(_ != 1)) Logger.error("Inter-level stencils are not allowed in operator generation")

          if (i + 1 < factors.indices.length && factors(i + 1).isInstanceOf[L2_FieldAccess]) {
            val field = factors(i + 1).asInstanceOf[L2_FieldAccess]
            val summands = sten.target.entries.map(_.asStencilOffsetEntry).map(entry => {
              val offsetField = Duplicate(field)
              offsetField.offsetWith(entry.offset)
              Duplicate(entry.coefficient) * offsetField : L2_Expression
            })
            newFactors += L2_Addition(summands)
            skipNext = true
            localChange = true
          } else {
            Logger.warn(s"Found stray stencil access when generating operators: ${ sten.prettyprint() }")
            newFactors += sten
          }

        case other => newFactors += other
      }

      if (localChange) {
        changed = true
        L2_Multiplication(newFactors)
      } else {
        mult
      }
  })

  this += new Transformation("Flatten nested multiplications", {
    // a * (b * c) => a * b * c
    case mult : L2_Multiplication =>
      val newSummands = ListBuffer[L2_Expression]()
      var localChange = false
      var negative = false
      for (ex <- mult.factors) ex match {
        case mult : L2_Multiplication =>
          localChange = true
          newSummands ++= mult.factors.map {
            case L2_Negative(ex2) =>
              negative = !negative
              ex2
            case ex2              => ex2
          }

        case L2_Negative(ex2) =>
          localChange = true
          negative = !negative
          newSummands += ex2

        case _ =>
          newSummands += ex
      }

      if (localChange) {
        changed = true
        if (negative)
          L2_Negative(L2_Multiplication(newSummands))
        else
          L2_Multiplication(newSummands)
      } else
        mult
  })

  this += new Transformation("Flatten nested additions", {
    // a + (b + c) => a + b + c
    case add : L2_Addition =>
      val newSummands = ListBuffer[L2_Expression]()
      var localChange = false
      for (ex <- add.summands) ex match {
        case add : L2_Addition =>
          localChange = true
          newSummands ++= add.summands

        case _ => newSummands += ex
      }

      if (localChange) {
        changed = true
        L2_Addition(newSummands)
      } else
        add
  })

  this += new Transformation("Resolve double negatives", {
    // -(-a) => a
    case L2_Negative(L2_Negative(ex)) =>
      changed = true
      ex
  })
}
