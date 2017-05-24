package exastencils.operator.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_StencilOps extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/operator/|LAYER_LC|/|LAYER_UC|_StencilOps.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.operator.|LAYER_LC|

import scala.collection.mutable._

import exastencils.base.|LAYER_LC|._
import exastencils.baseExt.|LAYER_LC|.|LAYER_UC|_FieldIteratorAccess
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.optimization.|LAYER_LC|._
import exastencils.util.|LAYER_LC|.|LAYER_UC|_ReplaceExpressions

/// |LAYER_UC|_StencilOps

object |LAYER_UC|_StencilOps {

  def add(left : |LAYER_UC|_Stencil, right : |LAYER_UC|_Stencil) : |LAYER_UC|_Stencil = {
    if (left.numDims != right.numDims) Logger.warn("Non-matching dimensionalities")
    if ((0 until left.numDims).map(i => left.colStride(i) != right.colStride(i)).reduce(_ || _)) Logger.warn("Non-matching colStrides")

    val newStencil = Duplicate.forceClone(left)
    newStencil.name += "_add_" + right.name
    newStencil.entries ++= Duplicate(right.entries)
    newStencil.squash()

    newStencil
  }

  def mul(left : |LAYER_UC|_Stencil, right : |LAYER_UC|_Stencil) : |LAYER_UC|_Stencil = {
    if (left.numDims != right.numDims) Logger.warn("Non-matching dimensionalities")
    // TODO: check potential level difference against colStride

    Logger.warn(s"Mul: ${ left.name } * ${ right.name }")

    val numDims = left.numDims

    val newStencil = |LAYER_UC|_Stencil(left.name + "_mul_" + right.name, left.level, numDims, left.colStride.indices.map(i => left.colStride(i) * right.colStride(i)).toArray, ListBuffer())

    for (left <- left.entries; right <- right.entries) {
      // (x, y) * (y, z) // (left.from, left.to) * (right.from, right.to)
      val newCol = Duplicate(right.col)
      for (d <- 0 until numDims) {
        |LAYER_UC|_ReplaceExpressions.toReplace = right.row.indices(d)
        |LAYER_UC|_ReplaceExpressions.replacement = left.col.indices(d)
        |LAYER_UC|_ReplaceExpressions.applyStandalone(newCol)
      }

      newStencil.entries += |LAYER_UC|_StencilMappingEntry(Duplicate(left.row), newCol, Duplicate(left.coefficient) * Duplicate(right.coefficient))
    }

    newStencil.entries.foreach(|LAYER_UC|_GeneralSimplify.doUntilDoneStandalone(_))

    newStencil.squash()
    newStencil.filter()
    newStencil
  }

  def scale(stencil : |LAYER_UC|_Stencil, factor : |LAYER_UC|_Expression) : |LAYER_UC|_Stencil = {
    val newStencil = Duplicate.forceClone(stencil)
    newStencil.name += "_scaled"
    newStencil.entries.foreach(_.coefficient *= factor)
    newStencil
  }

  def kron(left : |LAYER_UC|_Stencil, right : |LAYER_UC|_Stencil) : |LAYER_UC|_Stencil = {
    val otherCloned = Duplicate.forceClone(right)
    if (left.level != right.level) Logger.warn(s"Level mismatch: ${ left.level } vs ${ right.level }")
    val numDims = left.numDims

    object ShiftIteratorAccess extends DefaultStrategy("Replace something with something else") {
      var baseDim : Int = 0

      this += new Transformation("Search and replace", {
        case it : |LAYER_UC|_FieldIteratorAccess =>
          if (it.dim < baseDim) it.dim += baseDim
          it
      }, false)
    }

    ShiftIteratorAccess.baseDim = numDims
    ShiftIteratorAccess.applyStandalone(otherCloned.entries)

    val newStencil = |LAYER_UC|_Stencil(left.name + "_kron_" + otherCloned.name,
      left.level,
      numDims + otherCloned.numDims,
      Duplicate(left.colStride ++ otherCloned.colStride),
      left.entries.flatMap(l => otherCloned.entries.map(r =>
        Duplicate(|LAYER_UC|_StencilMappingEntry(
          |LAYER_UC|_ExpressionIndex(l.row.indices ++ r.row.indices),
          |LAYER_UC|_ExpressionIndex(l.col.indices ++ r.col.indices),
          l.coefficient * r.coefficient)))))

    newStencil.entries.foreach(|LAYER_UC|_GeneralSimplify.doUntilDoneStandalone(_))

    newStencil
  }

  def transpose(stencil : |LAYER_UC|_Stencil) : |LAYER_UC|_Stencil = {
    val newStencil = Duplicate.forceClone(stencil)
    newStencil.name += "_transposed"
    newStencil.colStride.transform(1.0 / _)

    newStencil.entries.transform(entry => {
      entry.transpose()
      for (d <- 0 until newStencil.numDims) {
        var done = false
        while (!done) {
          entry.row(d) match {
            case _ : |LAYER_UC|_FieldIteratorAccess =>
              // TODO: more precise matching
              done = true

            case add : |LAYER_UC|_Addition =>
              val (iterator, remainder) = add.summands.partition(e => StateManager.findFirst[|LAYER_UC|_FieldIteratorAccess]({ _ : |LAYER_UC|_FieldIteratorAccess => true }, |LAYER_UC|_ExpressionIndex(e)).isDefined)
              if (iterator.size != 1) Logger.error(s"unsupported: ${ iterator.size } != 1")

              entry.row(d) = iterator.head
              entry.col(d) = |LAYER_UC|_Subtraction(entry.col(d), |LAYER_UC|_Addition(remainder))

            case sub : |LAYER_UC|_Subtraction =>
              entry.row(d) = |LAYER_UC|_Addition(sub.left, |LAYER_UC|_Negative(sub.right))

            case mul : |LAYER_UC|_Multiplication =>
              val (iterator, remainder) = mul.factors.partition(e => StateManager.findFirst[|LAYER_UC|_FieldIteratorAccess]({ _ : |LAYER_UC|_FieldIteratorAccess => true }, |LAYER_UC|_ExpressionIndex(e)).isDefined)
              if (iterator.size != 1) Logger.error(s"unsupported: ${ iterator.size } != 1")

              entry.row(d) = iterator.head
              entry.col(d) = |LAYER_UC|_Division(entry.col(d), |LAYER_UC|_Multiplication(remainder))

            case div : |LAYER_UC|_Division =>
              if (StateManager.findFirst[|LAYER_UC|_FieldIteratorAccess]({ _ : |LAYER_UC|_FieldIteratorAccess => true }, div.left).isDefined) {
                entry.col(d) = |LAYER_UC|_Multiplication(entry.col(d), div.right)
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

    newStencil.entries.foreach(|LAYER_UC|_GeneralSimplify.doUntilDoneStandalone(_))

    newStencil
  }
}
"""
  }
}
