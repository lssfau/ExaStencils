package exastencils.operator.l3

import scala.collection.mutable._

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_FieldIteratorAccess
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.optimization.l3._
import exastencils.util.l3._

/// L3_StencilOps

object L3_StencilOps {

  def add(left : L3_Stencil, right : L3_Stencil) : L3_Stencil = {
    if (left.numDims != right.numDims) Logger.warn("Non-matching dimensionalities")
    if ((0 until left.numDims).map(i => left.colStride(i) != right.colStride(i)).reduce(_ || _)) Logger.warn("Non-matching colStrides")

    val newStencil = left.createDuplicate()
    newStencil.name += "_add_" + right.name
    newStencil.entries ++= Duplicate(right.entries)
    newStencil.squash()

    newStencil
  }

  def mul(left : L3_Stencil, right : L3_Stencil) : L3_Stencil = {
    if (left.numDims != right.numDims) Logger.warn("Non-matching dimensionalities")
    // TODO: check potential level difference against colStride

    Logger.warn(s"Mul: ${ left.name } * ${ right.name }")

    val numDims = left.numDims

    val newStencil = L3_Stencil(left.name + "_mul_" + right.name, left.level, numDims, left.colStride.indices.map(i => left.colStride(i) * right.colStride(i)).toArray, ListBuffer())

    for (left <- left.entries; right <- right.entries) {
      // (x, y) * (y, z) // (left.from, left.to) * (right.from, right.to)
      val newCol = Duplicate(right.col)
      for (d <- 0 until numDims) {
        L3_ReplaceExpressions.toReplace = right.row.indices(d)
        L3_ReplaceExpressions.replacement = left.col.indices(d)
        L3_ReplaceExpressions.applyStandalone(newCol)
      }

      newStencil.entries += L3_StencilMappingEntry(Duplicate(left.row), newCol, Duplicate(left.coefficient) * Duplicate(right.coefficient))
    }

    newStencil.entries.foreach(L3_GeneralSimplify.doUntilDoneStandalone(_))

    newStencil.squash()
    newStencil.filter()
    newStencil
  }

  def scale(stencil : L3_Stencil, factor : L3_Expression) : L3_Stencil = {
    val newStencil = stencil.createDuplicate()
    newStencil.name += "_scaled"
    newStencil.entries.foreach(_.coefficient *= factor)
    newStencil
  }

  def kron(left : L3_Stencil, right : L3_Stencil) : L3_Stencil = {
    val otherCloned = right.createDuplicate()
    if (left.level != right.level) Logger.warn(s"Level mismatch: ${ left.level } vs ${ right.level }")
    val numDims = left.numDims

    object ShiftIteratorAccess extends QuietDefaultStrategy("Replace something with something else") {
      var baseDim : Int = 0

      this += new Transformation("Search and replace", {
        case it : L3_FieldIteratorAccess =>
          if (it.dim < baseDim) it.dim += baseDim
          it
      }, false)
    }

    ShiftIteratorAccess.baseDim = numDims
    ShiftIteratorAccess.applyStandalone(otherCloned.entries)

    val newStencil = L3_Stencil(left.name + "_kron_" + otherCloned.name,
      left.level,
      numDims + otherCloned.numDims,
      Duplicate(left.colStride ++ otherCloned.colStride),
      left.entries.flatMap(l => otherCloned.entries.map(r =>
        Duplicate(L3_StencilMappingEntry(
          L3_ExpressionIndex(l.row.indices ++ r.row.indices),
          L3_ExpressionIndex(l.col.indices ++ r.col.indices),
          l.coefficient * r.coefficient)))))

    newStencil.entries.foreach(L3_GeneralSimplify.doUntilDoneStandalone(_))

    newStencil
  }

  def transpose(stencil : L3_Stencil) : L3_Stencil = {
    val newStencil = stencil.createDuplicate()
    newStencil.name += "_transposed"
    newStencil.colStride.transform(1.0 / _)

    newStencil.entries.transform(entry => {
      entry.transpose()
      for (d <- 0 until newStencil.numDims) {
        var done = false
        while (!done) {
          entry.row(d) match {
            case _ : L3_FieldIteratorAccess =>
              // TODO: more precise matching
              done = true

            case add : L3_Addition =>
              val (iterator, remainder) = add.summands.partition(e => StateManager.findFirst[L3_FieldIteratorAccess]({ _ : L3_FieldIteratorAccess => true }, L3_ExpressionIndex(e)).isDefined)
              if (iterator.size != 1) Logger.error(s"unsupported: ${ iterator.size } != 1")

              entry.row(d) = iterator.head
              entry.col(d) = L3_Subtraction(entry.col(d), L3_Addition(remainder))

            case sub : L3_Subtraction =>
              entry.row(d) = L3_Addition(sub.left, L3_Negative(sub.right))

            case mul : L3_Multiplication =>
              val (iterator, remainder) = mul.factors.partition(e => StateManager.findFirst[L3_FieldIteratorAccess]({ _ : L3_FieldIteratorAccess => true }, L3_ExpressionIndex(e)).isDefined)
              if (iterator.size != 1) Logger.error(s"unsupported: ${ iterator.size } != 1")

              entry.row(d) = iterator.head
              entry.col(d) = L3_Division(entry.col(d), L3_Multiplication(remainder))

            case div : L3_Division =>
              if (StateManager.findFirst[L3_FieldIteratorAccess]({ _ : L3_FieldIteratorAccess => true }, div.left).isDefined) {
                entry.col(d) = L3_Multiplication(entry.col(d), div.right)
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

    newStencil.entries.foreach(L3_GeneralSimplify.doUntilDoneStandalone(_))

    newStencil
  }

  def filterForSpecCase(stencil : L3_Stencil, c : ListBuffer[Int]) : L3_Stencil = {
    val newStencil = stencil.createDuplicate()
    newStencil.name += "_filtered_" + c.mkString("_")

    newStencil.entries = newStencil.entries.filter(entry => {
      // filter entries with invalid indices
      for (d <- 0 until stencil.numDims) {
        L3_ReplaceExpressions.toReplace = entry.row.indices(d)
        L3_ReplaceExpressions.replacement = c(d)
        L3_ReplaceExpressions.applyStandalone(entry.col)
      }

      entry.col.indices.map(L3_SimplifyExpression.simplifyFloatingExpr(_) match {
        case L3_RealConstant(v) => v.isValidInt
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
