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
  //TODO register collector
  // collector to find initialization expressions of matrices to classify
  var variableCollector = new IR_MatrixVarCollector()
  this.register(variableCollector)
  this.onBefore = () => {

    this.resetCollectors()
  }

  def computeMinMaxIndex(solve : IR_LocalSolve, numDimensions : Int) : (IR_ConstIndex, IR_ConstIndex) = {
    val minIndex = IR_ConstIndex(Array.fill(numDimensions)(Int.MinValue))
    val maxIndex = IR_ConstIndex(Array.fill(numDimensions)(Int.MaxValue))

    solve.unknowns.foreach(fa => {
      val layout = fa.field.layout
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

  def tryPrecomputingInverse(loop : IR_LoopOverDimensions, solve : IR_LocalSolve) : IR_Statement = {
    // check if constant matrix can be extracted
    if (solve.matrixIsConst) {
      solve.AInv = solve.getMatrix.inverse
      loop
    } else if (solve.matrixIsPosIndependent) {
      val matrix = solve.getMatrix
      val invMat = IR_VariableAccess("_AInv", Duplicate(matrix.datatype))
      val invMatDecl = IR_VariableDeclaration(invMat, IR_FunctionCall("inverse", Duplicate(matrix)))

      solve.AInv = invMat

      IR_Scope(invMatDecl, loop)
    } else {
      loop
    }
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
    if (Knowledge.experimental_forceOmitCondInLocalSolve
      || (minIndex.toExpressionIndex == loop.indices.begin && maxIndex.toExpressionIndex == loop.indices.end)) {
      solve.omitConditions = true
      return tryPrecomputingInverse(loop, solve)
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
    innerLoop.indices.begin.indices.transform(IR_SimplifyExpression.simplifyIntegralExpr(_))
    innerLoop.indices.end.indices.transform(IR_SimplifyExpression.simplifyIntegralExpr(_))

    innerLoop.body.head.asInstanceOf[IR_LocalSolve].omitConditions = true

    List(haloLoop, tryPrecomputingInverse(innerLoop, innerLoop.body.head.asInstanceOf[IR_LocalSolve]))
  }

  this += new Transformation("Split loops containing local solve nodes", {
    // check loop even if Knowledge.solver_splitLocalSolveLoops is false - conditions might still be unnecessary
    case loop : IR_LoopOverDimensions if loop.body.exists(_.isInstanceOf[IR_LocalSolve]) => handleLoop(loop)
  }, false)

  this += new Transformation("Perform expand for applicable nodes", {
    case solve : IR_LocalSolve                         => solve.expandSpecial
    case sls @ IR_SolveMatrixSystem(localSysMat, _, _, _) =>
      val sysMatAsExpr : IR_MatrixExpression = localSysMat match {
        case x : IR_MatrixExpression =>
          // to classify and const -> classify
          if (Knowledge.experimental_classifyLocMat) {
            x.shape = Some(IR_ClassifyMatShape(x))
            x
          }
          // const and shape not to classify
          else x
        // else: variable access: find initialization expression in declaration
        case va : IR_VariableAccess =>
          //TODO classification
          val initOpt : Option[IR_Expression] = variableCollector.getConstInitVal(va.name)
          // compiletime LU most likely does not work without pivoting -> use initial here too
          if(Knowledge.experimental_resolveLocalMatSys == "Compiletime") {
            if (initOpt.isEmpty) {
              Logger.warn("Compiletime LU without effective pivoting will most likely fail, switching back to inversion with cofactors!")
              val A = IR_MatNodeUtils.accessToMatExpr(va)
              A.annotate("SolveMatSys:fallback_inverse")
              A
            } else {
              if (Knowledge.experimental_matrixDebugConfig)
                Logger.warn("pivoting initial expression for solveMatSy ")
              val init = initOpt.get
              init.asInstanceOf[IR_MatrixExpression]
            }
          }
          else if (Knowledge.experimental_resolveLocalMatSys == "Runtime")
              IR_MatNodeUtils.accessToMatExpr(va)
          else Logger.error("something unexpected occurred")
      }
      sls.expand(sysMatAsExpr)
  })
}
