package exastencils.communication.ir

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.core.Duplicate
import exastencils.optimization.ir.IR_GeneralSimplify

/// QuadraticBaseShift

sealed case class QuadraticBaseShifts(var remapped : Boolean) extends BaseShifts {
  def toArray : Array[Int] = if (remapped) Array(0, 1, 2) else Array(-1, 0, 1)
}

/// QuadraticBasePositions

sealed case class QuadraticBasePositions(var x0 : IR_Expression, var x1 : IR_Expression, var x2 : IR_Expression) extends BasePositions {
  private val asArray : Array[IR_Expression] = Array(Duplicate(x0), Duplicate(x1), Duplicate(x2))

  import IR_InterpPackingCaches._

  def computeWeights(pos : IR_Expression) : BaseWeights = {
    if (!areWeightsCached(pos, this)) {

      def factors(j : Int) = {
        for (k <- asArray.indices if k != j) yield (pos - asArray(k)) / (asArray(j) - asArray(k)) : IR_Expression
      }

      val weights = asArray.indices.map(i => IR_Multiplication(factors(i) : _*))
      weights.map(e => {
        val wrapped = IR_ExpressionStatement(Duplicate(e))
        IR_GeneralSimplify.doUntilDoneStandalone(wrapped)
        wrapped.expression
      }).toArray

      val w = QuadraticBaseWeights(weights(0), weights(1), weights(2))
      addWeightsToCache(pos, this, w)
      w
    } else {
      getWeightsFromCache(pos, this)
    }
  }

  def toArray : Array[IR_Expression] = asArray
}

/// QuadraticBaseWeights

sealed case class QuadraticBaseWeights(var w0 : IR_Expression, var w1 : IR_Expression, var w2 : IR_Expression) extends BaseWeights {
  private val asArray : Array[IR_Expression] = Array(Duplicate(w0), Duplicate(w1), Duplicate(w2))

  def toArray : Array[IR_Expression] = asArray
}

/// QuadraticBaseValues

sealed case class QuadraticBaseValues(var f0 : IR_Expression, var f1 : IR_Expression, var f2 : IR_Expression) extends BaseValues {
  private val asArray : Array[IR_Expression] = Array(Duplicate(f0), Duplicate(f1), Duplicate(f2))

  def toArray : Array[IR_Expression] = asArray
}
