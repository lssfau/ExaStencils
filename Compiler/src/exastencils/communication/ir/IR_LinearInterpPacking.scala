package exastencils.communication.ir

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.optimization.ir.IR_GeneralSimplify

/// LinearBaseShifts

sealed case class LinearBaseShifts(var a0 : Int, var a1 : Int) extends BaseShifts {
  def toArray : Array[Int] = Array(a0, a1)
}

/// LinearBasePositions

sealed case class LinearBasePositions(var x0 : IR_Expression, var x1 : IR_Expression) extends BasePositions {
  private val asArray : Array[IR_Expression] = Array(Duplicate(x0), Duplicate(x1))

  def computeWeights(pos : IR_Expression) : BaseWeights = {
    LinearBaseWeights((pos - x1) / (x0 - x1), (pos - x0) / (x1 - x0))
  }

  def toArray : Array[IR_Expression] = asArray
}

/// LinearBaseWeights

sealed case class LinearBaseWeights(var w0 : IR_Expression, var w1 : IR_Expression) extends BaseWeights {
  private val asArray : Array[IR_Expression] = Array(Duplicate(w0), Duplicate(w1))

  def toArray : Array[IR_Expression] = asArray
}

/// LinearBaseValues

sealed case class LinearBaseValues(var f0 : IR_Expression, var f1 : IR_Expression) extends BaseValues {
  private val asArray : Array[IR_Expression] = Array(Duplicate(f0), Duplicate(f1))

  def toArray : Array[IR_Expression] = asArray
}
