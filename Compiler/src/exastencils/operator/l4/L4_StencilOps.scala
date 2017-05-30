package exastencils.operator.l4

import scala.collection.mutable._

import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_FieldIteratorAccess
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.optimization.l4._
import exastencils.util.l4._

/// L4_StencilOps

object L4_StencilOps {

  def add(left : L4_Stencil, right : L4_Stencil) : L4_Stencil = {
    if (left.numDims != right.numDims) Logger.warn("Non-matching dimensionalities")
    if ((0 until left.numDims).map(i => left.colStride(i) != right.colStride(i)).reduce(_ || _)) Logger.warn("Non-matching colStrides")

    val newStencil = Duplicate.forceClone(left)
    newStencil.name += "_add_" + right.name
    newStencil.entries ++= Duplicate(right.entries)
    newStencil.squash()

    newStencil
  }

  def mul(left : L4_Stencil, right : L4_Stencil) : L4_Stencil = {
    if (left.numDims != right.numDims) Logger.warn("Non-matching dimensionalities")
    // TODO: check potential level difference against colStride

    Logger.warn(s"Mul: ${ left.name } * ${ right.name }")

    val numDims = left.numDims

    val newStencil = L4_Stencil(left.name + "_mul_" + right.name, left.level, numDims, left.colStride.indices.map(i => left.colStride(i) * right.colStride(i)).toArray, ListBuffer())

    for (left <- left.entries; right <- right.entries) {
      // (x, y) * (y, z) // (left.from, left.to) * (right.from, right.to)
      val newCol = Duplicate(right.col)
      for (d <- 0 until numDims) {
        L4_ReplaceExpressions.toReplace = right.row.indices(d)
        L4_ReplaceExpressions.replacement = left.col.indices(d)
        L4_ReplaceExpressions.applyStandalone(newCol)
      }

      newStencil.entries += L4_StencilMappingEntry(Duplicate(left.row), newCol, Duplicate(left.coefficient) * Duplicate(right.coefficient))
    }

    newStencil.entries.foreach(L4_GeneralSimplify.doUntilDoneStandalone(_))

    newStencil.squash()
    newStencil.filter()
    newStencil
  }

  def scale(stencil : L4_Stencil, factor : L4_Expression) : L4_Stencil = {
    val newStencil = Duplicate.forceClone(stencil)
    newStencil.name += "_scaled"
    newStencil.entries.foreach(_.coefficient *= factor)
    newStencil
  }

  def kron(left : L4_Stencil, right : L4_Stencil) : L4_Stencil = {
    val otherCloned = Duplicate.forceClone(right)
    if (left.level != right.level) Logger.warn(s"Level mismatch: ${ left.level } vs ${ right.level }")
    val numDims = left.numDims

    object ShiftIteratorAccess extends DefaultStrategy("Replace something with something else") {
      var baseDim : Int = 0

      this += new Transformation("Search and replace", {
        case it : L4_FieldIteratorAccess =>
          if (it.dim < baseDim) it.dim += baseDim
          it
      }, false)
    }

    ShiftIteratorAccess.baseDim = numDims
    ShiftIteratorAccess.applyStandalone(otherCloned.entries)

    val newStencil = L4_Stencil(left.name + "_kron_" + otherCloned.name,
      left.level,
      numDims + otherCloned.numDims,
      Duplicate(left.colStride ++ otherCloned.colStride),
      left.entries.flatMap(l => otherCloned.entries.map(r =>
        Duplicate(L4_StencilMappingEntry(
          L4_ExpressionIndex(l.row.indices ++ r.row.indices),
          L4_ExpressionIndex(l.col.indices ++ r.col.indices),
          l.coefficient * r.coefficient)))))

    newStencil.entries.foreach(L4_GeneralSimplify.doUntilDoneStandalone(_))

    newStencil
  }

  def transpose(stencil : L4_Stencil) : L4_Stencil = {
    val newStencil = Duplicate.forceClone(stencil)
    newStencil.name += "_transposed"
    newStencil.colStride.transform(1.0 / _)

    newStencil.entries.transform(entry => {
      entry.transpose()
      for (d <- 0 until newStencil.numDims) {
        var done = false
        while (!done) {
          entry.row(d) match {
            case _ : L4_FieldIteratorAccess =>
              // TODO: more precise matching
              done = true

            case add : L4_Addition =>
              val (iterator, remainder) = add.summands.partition(e => StateManager.findFirst[L4_FieldIteratorAccess]({ _ : L4_FieldIteratorAccess => true }, L4_ExpressionIndex(e)).isDefined)
              if (iterator.size != 1) Logger.error(s"unsupported: ${ iterator.size } != 1")

              entry.row(d) = iterator.head
              entry.col(d) = L4_Subtraction(entry.col(d), L4_Addition(remainder))

            case sub : L4_Subtraction =>
              entry.row(d) = L4_Addition(sub.left, L4_Negative(sub.right))

            case mul : L4_Multiplication =>
              val (iterator, remainder) = mul.factors.partition(e => StateManager.findFirst[L4_FieldIteratorAccess]({ _ : L4_FieldIteratorAccess => true }, L4_ExpressionIndex(e)).isDefined)
              if (iterator.size != 1) Logger.error(s"unsupported: ${ iterator.size } != 1")

              entry.row(d) = iterator.head
              entry.col(d) = L4_Division(entry.col(d), L4_Multiplication(remainder))

            case div : L4_Division =>
              if (StateManager.findFirst[L4_FieldIteratorAccess]({ _ : L4_FieldIteratorAccess => true }, div.left).isDefined) {
                entry.col(d) = L4_Multiplication(entry.col(d), div.right)
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

    newStencil.entries.foreach(L4_GeneralSimplify.doUntilDoneStandalone(_))

    newStencil
  }

  def filterForSpecCase(stencil : L4_Stencil, c : ListBuffer[Int]) : L4_Stencil = {
    val newStencil = Duplicate.forceClone(stencil)
    newStencil.name += "_filtered_" + c.mkString("_")

    newStencil.entries = newStencil.entries.filter(entry => {
      // filter entries with invalid indices
      for (d <- 0 until stencil.numDims) {
        L4_ReplaceExpressions.toReplace = entry.row.indices(d)
        L4_ReplaceExpressions.replacement = c(d)
        L4_ReplaceExpressions.applyStandalone(entry.col)
      }

      entry.col.indices.map(L4_SimplifyExpression.simplifyFloatingExpr(_) match {
        case L4_RealConstant(v) => v.isValidInt
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
