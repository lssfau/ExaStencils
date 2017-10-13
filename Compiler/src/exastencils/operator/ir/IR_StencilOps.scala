package exastencils.operator.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FieldIteratorAccess
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.util.ir._

/// IR_StencilOps

object IR_StencilOps {

  def add(left : IR_Stencil, right : IR_Stencil) : IR_Stencil = {
    if (left.numDims != right.numDims) Logger.warn("Non-matching dimensionalities")
    if ((0 until left.numDims).map(i => left.colStride(i) != right.colStride(i)).reduce(_ || _)) Logger.warn("Non-matching colStrides")

    val newStencil = Duplicate.forceClone(left)
    newStencil.name += "_add_" + right.name
    newStencil.entries ++= Duplicate(right.entries)
    newStencil.squash()

    newStencil
  }

  def mul(left : IR_Stencil, right : IR_Stencil) : IR_Stencil = {
    if (left.numDims != right.numDims) Logger.warn("Non-matching dimensionalities")
    // TODO: check potential level difference against colStride

    Logger.warn(s"Mul: ${ left.name } * ${ right.name }")

    val numDims = left.numDims

    val newStencil = IR_Stencil(left.name + "_mul_" + right.name, left.level, numDims, left.colStride.indices.map(i => left.colStride(i) * right.colStride(i)).toArray, ListBuffer())

    for (left <- left.entries; right <- right.entries) {
      // (x, y) * (y, z) // (left.from, left.to) * (right.from, right.to)
      val newCol = Duplicate(right.col)
      for (d <- 0 until numDims) {
        IR_ReplaceExpressions.toReplace = right.row.indices(d)
        IR_ReplaceExpressions.replacement = left.col.indices(d)
        IR_ReplaceExpressions.applyStandalone(newCol)
      }

      newStencil.entries += IR_StencilMappingEntry(Duplicate(left.row), newCol, Duplicate(left.coefficient) * Duplicate(right.coefficient))
    }

    newStencil.entries.foreach(IR_GeneralSimplify.doUntilDoneStandalone(_))

    newStencil.squash()
    newStencil.filter()
    newStencil
  }

  def scale(stencil : IR_Stencil, factor : IR_Expression) : IR_Stencil = {
    val newStencil = Duplicate.forceClone(stencil)
    newStencil.name += "_scaled"
    newStencil.entries.foreach(_.coefficient *= factor)
    newStencil
  }

  def kron(left : IR_Stencil, right : IR_Stencil) : IR_Stencil = {
    val otherCloned = Duplicate.forceClone(right)
    if (left.level != right.level) Logger.warn(s"Level mismatch: ${ left.level } vs ${ right.level }")
    val numDims = left.numDims

    object ShiftIteratorAccess extends QuietDefaultStrategy("Replace something with something else") {
      var baseDim : Int = 0

      this += new Transformation("Search and replace", {
        case it : IR_FieldIteratorAccess =>
          if (it.dim < baseDim) it.dim += baseDim
          it
      }, false)
    }

    ShiftIteratorAccess.baseDim = numDims
    ShiftIteratorAccess.applyStandalone(otherCloned.entries)

    val newStencil = IR_Stencil(left.name + "_kron_" + otherCloned.name,
      left.level,
      numDims + otherCloned.numDims,
      Duplicate(left.colStride ++ otherCloned.colStride),
      left.entries.flatMap(l => otherCloned.entries.map(r =>
        Duplicate(IR_StencilMappingEntry(
          IR_ExpressionIndex(l.row.indices ++ r.row.indices),
          IR_ExpressionIndex(l.col.indices ++ r.col.indices),
          l.coefficient * r.coefficient)))))

    newStencil.entries.foreach(IR_GeneralSimplify.doUntilDoneStandalone(_))

    newStencil
  }

  def transpose(stencil : IR_Stencil) : IR_Stencil = {
    val newStencil = Duplicate.forceClone(stencil)
    newStencil.name += "_transposed"
    newStencil.colStride.transform(1.0 / _)

    newStencil.entries.transform(entry => {
      entry.transpose()
      for (d <- 0 until newStencil.numDims) {
        var done = false
        while (!done) {
          entry.row(d) match {
            case _ : IR_FieldIteratorAccess =>
              // TODO: more precise matching
              done = true

            case add : IR_Addition =>
              val (iterator, remainder) = add.summands.partition(e => StateManager.findFirst[IR_FieldIteratorAccess]({ _ : IR_FieldIteratorAccess => true }, IR_ExpressionIndex(e)).isDefined)
              if (iterator.size != 1) Logger.error(s"unsupported: ${ iterator.size } != 1")

              entry.row(d) = iterator.head
              entry.col(d) = IR_Subtraction(entry.col(d), IR_Addition(remainder))

            case sub : IR_Subtraction =>
              entry.row(d) = IR_Addition(sub.left, IR_Negative(sub.right))

            case mul : IR_Multiplication =>
              val (iterator, remainder) = mul.factors.partition(e => StateManager.findFirst[IR_FieldIteratorAccess]({ _ : IR_FieldIteratorAccess => true }, IR_ExpressionIndex(e)).isDefined)
              if (iterator.size != 1) Logger.error(s"unsupported: ${ iterator.size } != 1")

              entry.row(d) = iterator.head
              entry.col(d) = IR_Division(entry.col(d), IR_Multiplication(remainder))

            case div : IR_Division =>
              if (StateManager.findFirst[IR_FieldIteratorAccess]({ _ : IR_FieldIteratorAccess => true }, div.left).isDefined) {
                entry.col(d) = IR_Multiplication(entry.col(d), div.right)
                entry.row(d) = div.left
              } else {
                Logger.error("unsupported")
              }

            case other => Logger.error(other)
          }
        }
      }
      entry
    })

    newStencil.entries.foreach(IR_GeneralSimplify.doUntilDoneStandalone(_))

    newStencil
  }

  def filterForSpecCase(stencil : IR_Stencil, c : ListBuffer[Int]) : IR_Stencil = {
    val newStencil = Duplicate.forceClone(stencil)
    newStencil.name += "_filtered_" + c.mkString("_")

    newStencil.entries = newStencil.entries.filter(entry => {
      // filter entries with invalid indices
      for (d <- 0 until stencil.numDims) {
        IR_ReplaceExpressions.toReplace = entry.row.indices(d)
        IR_ReplaceExpressions.replacement = c(d)
        IR_ReplaceExpressions.applyStandalone(entry.col)
      }

      entry.col.indices.map(IR_SimplifyExpression.simplifyFloatingExpr(_) match {
        case IR_RealConstant(v) => v.isValidInt
        case other              => Logger.warn(other); false
      }).reduce(_ && _)
    })

    // re-construct stencil entries
    newStencil.entries.foreach(entry => {
      val base = Duplicate(entry.row)
      (0 until entry.numDims).foreach(d => base(d) *= newStencil.colStride(d))
      entry.col += base
    })

    newStencil
  }
}
