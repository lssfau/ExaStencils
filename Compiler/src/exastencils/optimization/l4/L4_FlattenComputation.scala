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

package exastencils.optimization.l4

import scala.collection.mutable._

import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._

/// L4_FlattenComputation

object L4_FlattenComputation extends QuietDefaultStrategy("Flatten complex computation expressions") {
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
    case sub : L4_Subtraction =>
      changed = true
      val add = L4_Addition(sub.left)
      add.summands += L4_Negative(sub.right)
      add
  })

  this += new Transformation("Resolve additions nested in multiplications", {
    // (a + b) * (c + d) => a*c + a*d + b*c + b*d
    case mult : L4_Multiplication =>
      var nonAdditions = mult.factors
      var additions = ListBuffer[L4_Addition]()
      for (ex <- mult.factors) ex match {
        case add : L4_Addition =>
          nonAdditions -= add
          additions += add
        case _                 =>
      }

      if (additions.nonEmpty) {
        changed = true
        val ret = L4_Addition()
        for (add <- additions) {
          if (ret.summands.isEmpty)
            ret.summands = add.summands.map(s => L4_Multiplication(s +: Duplicate(nonAdditions)))
          else
            ret.summands = add.summands.flatMap(s => ret.summands.map(r => L4_Multiplication(Duplicate(s), Duplicate(r))))
        }

        ret
      } else
        mult
  })

  this += new Transformation("Split divisions with additions and multiplications in the numerator", {
    // (a + b) / c => a/c + b/c
    case L4_Division(add : L4_Addition, div) =>
      changed = true
      L4_Addition(add.summands.map(ex => L4_Division(ex, Duplicate(div)) : L4_Expression))

    // (a * b) / c => a*c + b*c
    case L4_Division(mult : L4_Multiplication, div) =>
      changed = true
      mult.factors += L4_Division(1.0, Duplicate(div))
      mult
  })

  this += new Transformation("Flatten nested multiplications", {
    // a * (b * c) => a * b * c
    case mult : L4_Multiplication =>
      val newSummands = ListBuffer[L4_Expression]()
      var localChange = false
      var negative = false
      for (ex <- mult.factors) ex match {
        case mult : L4_Multiplication =>
          localChange = true
          newSummands ++= mult.factors.map {
            case L4_Negative(ex2) =>
              negative = !negative
              ex2
            case ex2              => ex2
          }

        case L4_Negative(ex2) =>
          localChange = true
          negative = !negative
          newSummands += ex2

        case _ =>
          newSummands += ex
      }

      if (localChange) {
        changed = true
        if (negative)
          L4_Negative(L4_Multiplication(newSummands))
        else
          L4_Multiplication(newSummands)
      } else
        mult
  })

  this += new Transformation("Flatten nested additions", {
    // a + (b + c) => a + b + c
    case add : L4_Addition =>
      val newSummands = ListBuffer[L4_Expression]()
      var localChange = false
      for (ex <- add.summands) ex match {
        case add : L4_Addition =>
          localChange = true
          newSummands ++= add.summands

        case _ => newSummands += ex
      }

      if (localChange) {
        changed = true
        L4_Addition(newSummands)
      } else
        add
  })

  this += new Transformation("Resolve double negatives", {
    // -(-a) => a
    case L4_Negative(L4_Negative(ex)) =>
      changed = true
      ex
  })
}
