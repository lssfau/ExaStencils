package exastencils.solver.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.grid.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir._

/// IR_ResolveLocalSolve

object IR_ResolveLocalSolve extends DefaultStrategy("Resolve IR_LocalSolve nodes") {
  def computeMinMaxIndex(solve : IR_LocalSolve, numDimensions : Int) : (IR_ConstIndex, IR_ConstIndex) = {
    val minIndex = IR_ConstIndex(Array.fill(numDimensions)(Int.MinValue))
    val maxIndex = IR_ConstIndex(Array.fill(numDimensions)(Int.MaxValue))

    solve.unknowns.foreach(fa => {
      val layout = fa.fieldSelection.fieldLayout
      val offset = fa.getOffsetFromIndex
      val refOffset = layout.referenceOffset.toConstIndex

      layout.localization match {
        case IR_AtNode =>
          for (dim <- 0 until numDimensions) {
            minIndex(dim) = math.max(minIndex(dim), layout.defIdxInnerBegin(dim) - refOffset(dim) - offset(dim))
            maxIndex(dim) = math.min(maxIndex(dim), layout.defIdxInnerEnd(dim) - refOffset(dim) - offset(dim))
          }

        case IR_AtFaceCenter(fDim) =>
          for (dim <- 0 until numDimensions) {
            if (dim == fDim) {
              minIndex(dim) = math.max(minIndex(dim), layout.defIdxInnerBegin(dim) - refOffset(dim) - offset(dim))
              maxIndex(dim) = math.min(maxIndex(dim), layout.defIdxInnerEnd(dim) - refOffset(dim) - offset(dim))
            } else {
              minIndex(dim) = math.max(minIndex(dim), layout.defIdxDupLeftBegin(dim) - refOffset(dim) - offset(dim))
              maxIndex(dim) = math.min(maxIndex(dim), layout.defIdxDupRightEnd(dim) - refOffset(dim) - offset(dim))
            }
          }

        case IR_AtCellCenter =>
          for (dim <- 0 until numDimensions) {
            minIndex(dim) = math.max(minIndex(dim), layout.defIdxDupLeftBegin(dim) - refOffset(dim) - offset(dim))
            maxIndex(dim) = math.min(maxIndex(dim), layout.defIdxDupRightEnd(dim) - refOffset(dim) - offset(dim))
          }
      }
    })

    (minIndex, maxIndex)
  }

  def handleLoop(loop : IR_LoopOverDimensions) : Transformation.OutputType = {
    if (loop.body.length > 1) {
      Logger.warn("Found IR_LocalSolve in loop but there are other statements in the body. Currently unsupported.")
      return loop
    }

    // get solve node
    val solve = loop.body.head.asInstanceOf[IR_LocalSolve]

    // compute min and max index of condition free iteration space
    val (minIndex, maxIndex) = computeMinMaxIndex(solve, loop.numDimensions)

    // return base loop if halo Loop is not required
    if (minIndex.toExpressionIndex == loop.indices.begin && maxIndex.toExpressionIndex == loop.indices.end) {
      solve.omitConditions = true
      return loop
    }

    // abort if splitting is not allowed
    if (!Knowledge.solver_splitLocalSolveLoops)
      return loop

    // set up halo and inner loop
    val haloLoop = Duplicate(loop)
    val haloCond = (0 until loop.numDimensions).map(dim => IR_FieldIteratorAccess(dim) < minIndex(dim) OrOr IR_FieldIteratorAccess(dim) >= maxIndex(dim)).reduce(_ OrOr _)
    haloLoop.condition = Some(haloLoop.condition.getOrElse(IR_BooleanConstant(true)) AndAnd haloCond)

    val innerLoop = Duplicate(loop)
    // fuse indices with the new index range
    for (i <- innerLoop.indices.begin.indices.indices)
      innerLoop.indices.begin(i) = IR_Maximum(innerLoop.indices.begin(i), minIndex.indices(i))
    for (i <- innerLoop.indices.end.indices.indices)
      innerLoop.indices.end(i) = IR_Minimum(innerLoop.indices.end(i), maxIndex.indices(i))

    // simplify loop boundaries
    innerLoop.indices.begin.indices.transform(IR_SimplifyExpression.simplifyIntegralExpr)
    innerLoop.indices.end.indices.transform(IR_SimplifyExpression.simplifyIntegralExpr)

    innerLoop.body.head.asInstanceOf[IR_LocalSolve].omitConditions = true

    List(haloLoop, innerLoop)
  }

  this += new Transformation("Split loops containing local solve nodes", {
    // check loop even if Knowledge.solver_splitLocalSolveLoops is false - conditions might still be unnecessary
    case loop : IR_LoopOverDimensions if loop.body.exists(_.isInstanceOf[IR_LocalSolve]) => handleLoop(loop)
  })

  this += new Transformation("Perform expandSpecial for applicable nodes", {
    case solve : IR_LocalSolve => solve.expandSpecial
  })
}