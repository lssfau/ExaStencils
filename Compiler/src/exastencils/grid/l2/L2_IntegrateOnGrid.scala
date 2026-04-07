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

package exastencils.grid.l2

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.baseExt.l2.L2_FieldIteratorAccess
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.datastructures._
import exastencils.field.l2.L2_FieldAccess
import exastencils.grid.l3.L3_IntegrateOnGrid
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l2.L2_CollectFieldAccesses

/// L2_IntegrateOnGrid

object L2_IntegrateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[L2_Expression], offset : Option[L2_ConstIndex]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      L2_RealConstant(0.0)
    } else {
      if (args.length > 1) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      new L2_IntegrateOnGrid(name, level, args(0), offset)
    }
  }
}

case class L2_IntegrateOnGrid(
    var name : String,
    var level : Int,
    var expression : L2_Expression,
    var offset : Option[L2_ConstIndex]) extends L2_Expression with L2_CanBeOffset with L2_MayBlockResolution {

  allDone = !Knowledge.experimental_l2_resolveVirtualFields

  def numDims = /*FIXME*/ Knowledge.dimensionality
  def stagDim = L2_GridUtil.faceToDims(name.replace("integrateOver", ""))._1
  def faceDim = L2_GridUtil.faceToDims(name.replace("integrateOver", ""))._2

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << offset.get
    out << " ( " << expression << " ) "
  }

  def getEffectiveOffset = {
    val effectiveOffset = offset.getOrElse(L2_ConstIndex(Array.fill(numDims)(0)))

    // take into account that right-hand interfaces can be targets too
    val faceOffset = L2_GridUtil.offsetForFace(name.replace("integrateOver", ""))
    L2_GridUtil.offsetIndex(effectiveOffset, faceOffset, faceDim)
  }

  def doIntegrate(exp : L2_Expression) = {
    var index = L2_FieldIteratorAccess.fullIndex(numDims)
    index += getEffectiveOffset

    // calculate size of integration domain (area)
    def cellInterfaceFor(dim : Int) =
      if (stagDim.isEmpty)  // integrate over regular cell interface
        L2_VF_CellWidthPerDim.access(level, dim, Duplicate(index))
      else // integrate over staggered cell interface
        L2_VF_StagCellWidthPerDim.access(level, stagDim.get, dim, Duplicate(index))

    def area = (0 until numDims).filter(_ != faceDim).map(dim => cellInterfaceFor(dim) : L2_Expression).reduce(_ * _)

    // define offset expression
    def offsetExp = {
      val wrapped = L2_ExpressionStatement(Duplicate(exp))
      L2_OffsetAllApplicable.offset = getEffectiveOffset
      L2_OffsetAllApplicable.applyStandalone(wrapped)
      wrapped.expression
    }

    // TODO: shift area?
    area * offsetExp
  }

  def resolve() : L2_Expression = {
    if (!Knowledge.grid_isAxisAligned) Logger.error("Non axis-aligned grids are currently not supported for integration")

    val wrapped = L2_ExpressionStatement(expression) // for matching purposes

    // collect (virtual) field accesses
    L2_CollectFieldAccesses.applyStandalone(wrapped)

    // check if all occurring level specifications are identical
    if (L2_CollectFieldAccesses.fieldAccesses.map(_.level).exists(_ != level)
      || L2_CollectFieldAccesses.vFieldAccesses.map(_.level).exists(_ != level))
      Logger.error(s"Mixed level integration is currently not supported (${ expression.prettyprint })")

    // if there are no field accesses direct integration is possible
    if (0 == L2_CollectFieldAccesses.fieldAccesses.length)
      return doIntegrate(expression)

    /// step 1: wrap field accesses with eval functions and mark them for piecewise integration if necessary

    object L2_WrapFieldAccessesForIntegration extends QuietDefaultStrategy("Wrap field accesses with evaluation nodes") {
      val pIntAnnot = "PIECEWISE_INTEGRATION" // +0 and +1 in the given dimension for piecewise integrations
      def addPIntAnnot(dim : Int, exp : L2_Expression) = { exp.annotate(s"${ pIntAnnot }_$dim"); exp }

      this += new Transformation("Wrap", {
        case fieldAccess : L2_FieldAccess =>
          if (stagDim.isEmpty) { // non-staggered cells
            fieldAccess.target.localization match {
              case L2_AtCellCenter => // interpolation
                L2_EvaluateOnGrid(stagDim, faceDim, level, fieldAccess)

              case L2_AtFaceCenter(fd) if faceDim == fd => // direct sampling
                fieldAccess

              case L2_AtFaceCenter(fd) => // interpolation, piecewiseIntegration
                addPIntAnnot(fd, L2_EvaluateOnGrid(stagDim, faceDim, level, fieldAccess))

              case other => Logger.error(s"Unknown or unsupported localization $other for field $fieldAccess in basic integration")
            }
          } else {
            // staggered cells
            val curStagDim = stagDim.get
            fieldAccess.target.localization match {
              case L2_AtCellCenter if curStagDim == faceDim => // direct sampling with offset
                L2_GridUtil.offsetAccess(fieldAccess, -1, curStagDim)

              case L2_AtCellCenter if curStagDim != faceDim => // interpolation with offset, piecewiseIntegration
                addPIntAnnot(curStagDim, L2_EvaluateOnGrid(None, faceDim, level, L2_GridUtil.offsetAccess(fieldAccess, -1, curStagDim)))

              case L2_AtFaceCenter(`curStagDim`) => // field localization matches CV -> interpolation
                L2_EvaluateOnGrid(stagDim, faceDim, level, fieldAccess)

              case L2_AtFaceCenter(fd) if faceDim == curStagDim => // direct sampling with offset, piecewiseIntegration
                addPIntAnnot(fd, L2_GridUtil.offsetAccess(fieldAccess, -1, curStagDim))

              case L2_AtFaceCenter(fd) if faceDim == fd => // direct sampling with offset, piecewiseIntegration
                addPIntAnnot(curStagDim, L2_GridUtil.offsetAccess(fieldAccess, -1, curStagDim))

              case L2_AtFaceCenter(fd) => // interpolation with offset, double piecewiseIntegration
                addPIntAnnot(curStagDim, addPIntAnnot(fd,
                  L2_EvaluateOnGrid(stagDim, faceDim, level, L2_GridUtil.offsetAccess(fieldAccess, -1, curStagDim))))

              case other => Logger.error(s"Unknown or unsupported localization $other for field $fieldAccess in staggered integration")
            }
          }

        case fieldAccess : L2_VirtualFieldAccess =>
          Logger.error(s"Virtual field accesses ($fieldAccess) are currently unsupported within evaluation and integration functions")

        case eval : L2_EvaluateOnGrid =>
          if (eval.faceDim != faceDim) Logger.error(s"Found unaligned eval for faceDim ${ eval.faceDim } in integration for faceDim $faceDim in eval for ${ eval.expression }")
          if (eval.stagDim != stagDim) Logger.error(s"Found unaligned eval for stagDim ${ eval.stagDim } in integration for stagDim $stagDim in eval for ${ eval.expression }")

          // prevent double shift; occurs in, e.g., integrateRight ( evalRight ( exp ) )
          val evalOffset = L2_GridUtil.offsetForFace(eval.name.replace("evalAt", ""))
          val integrateOffset = L2_GridUtil.offsetForFace(L2_IntegrateOnGrid.this.name.replace("integrateOver", ""))
          if (evalOffset != integrateOffset) Logger.error("Nested evaluate and integrate face don't match")
          if (evalOffset != 0) eval.offsetWith(L2_GridUtil.offsetIndex(L2_ConstIndex(Array.fill(eval.numDims)(0)), -evalOffset, eval.faceDim))

          if (stagDim.isEmpty) {
            eval.fieldAccess().target.localization match {
              case L2_AtCellCenter => // interpolation (already sufficient)
                eval

              case L2_AtFaceCenter(fd) if faceDim == fd => // direct sampling
                eval.expression

              case L2_AtFaceCenter(fd) => // interpolation, piecewiseIntegration
                addPIntAnnot(fd, eval)

              case other => Logger.error(s"Unknown or unsupported localization $other for field ${ eval.fieldAccess() } in basic integration")
            }
          } else {
            val curStagDim = stagDim.get
            eval.fieldAccess().target.localization match {
              case L2_AtCellCenter if curStagDim == faceDim => // direct sampling with offset
                L2_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)

              case L2_AtCellCenter if curStagDim != faceDim => // interpolation with offset, piecewiseIntegration
                eval.name = s"evalAt${ L2_GridUtil.dimsToFace(None, faceDim) }" // evaluate at un-staggered cell interface
                eval.expression = L2_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                eval.expression = L2_GridUtil.offsetAccess(eval.fieldAccess(), 1, faceDim)
                addPIntAnnot(curStagDim, eval)

              case L2_AtFaceCenter(`curStagDim`) => // field localization matches CV -> interpolation
                eval

              case L2_AtFaceCenter(fd) if faceDim == curStagDim => // direct sampling with offset, piecewiseIntegration
                eval.expression = L2_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                addPIntAnnot(fd, eval)

              case L2_AtFaceCenter(fd) if faceDim == fd => // direct sampling with offset, piecewiseIntegration
                eval.expression = L2_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                addPIntAnnot(curStagDim, eval)

              case L2_AtFaceCenter(fd) => // interpolation with offset, double piecewiseIntegration
                eval.expression = L2_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                addPIntAnnot(curStagDim, addPIntAnnot(fd, eval))

              case other => Logger.error(s"Unknown or unsupported localization $other for field ${ eval.fieldAccess() } in staggered integration")
            }
          }

        case _ : L2_IntegrateOnGrid =>
          Logger.error("Integration functions called inside other integration functions are currently not supported")
      }, false) // not recursive -> don't look inside eval functions
    }
    L2_WrapFieldAccessesForIntegration.applyStandalone(wrapped)

    /// step 2: check if integration by parts is required

    val piecewiseIntegrationPerDim = (0 until numDims).map(dim => StateManager.findFirst({ n : Node =>
      n.annotations.contains(s"${ L2_WrapFieldAccessesForIntegration.pIntAnnot }_$dim")
    }, wrapped).isDefined)

    if (!piecewiseIntegrationPerDim.contains(true))
      return doIntegrate(wrapped.expression)

    /// step 3: apply chosen integration

    var index = L2_FieldIteratorAccess.fullIndex(numDims)
    index += getEffectiveOffset

    var result = Duplicate(wrapped.expression)

    for (dim <- 0 until numDims) {
      if (dim == faceDim) {
        // nothing to do
      } else if (!piecewiseIntegrationPerDim(dim)) {
        // no piecewise integration is required for the current dimension
        // -> simply multiply with the length of the interface in dim

        val length =
          if (stagDim.isEmpty)  // integrate over regular cell interface
            L2_VF_CellWidthPerDim.access(level, dim, Duplicate(index))
          else // integrate over staggered cell interface
            L2_VF_StagCellWidthPerDim.access(level, stagDim.get, dim, Duplicate(index))

        result = length * result
      } else {
        // piecewise integration is required for the current dimension

        val (lowerLength, upperLength) = stagDim match {
          case None        => // 'normal' cell -> piecewise integration across lower and upper half of the cell
            (0.5 * L2_VF_CellWidthPerDim.access(level, dim, Duplicate(index)),
              0.5 * L2_VF_CellWidthPerDim.access(level, dim, Duplicate(index)))
          case Some(`dim`) => // integration in the direction of the staggering -> half of the left cell and half of the current cell
            (0.5 * L2_VF_CellWidthPerDim.access(level, dim, L2_GridUtil.offsetIndex(index, -1, dim)),
              0.5 * L2_VF_CellWidthPerDim.access(level, dim, Duplicate(index)))
          case Some(_)     => // integration on a staggered cell, but not in the direction of the staggering -> handle like an unstaggered cell
            (0.5 * L2_VF_CellWidthPerDim.access(level, dim, Duplicate(index)),
              0.5 * L2_VF_CellWidthPerDim.access(level, dim, Duplicate(index)))
        }

        val lowerResult = L2_ExpressionStatement(result)
        val upperResult = L2_ExpressionStatement(Duplicate(result))
        L2_OffsetAllWithAnnotation.offset = L2_GridUtil.offsetIndex(L2_ConstIndex(Array.fill(numDims)(0)), 1, dim)
        L2_OffsetAllWithAnnotation.requiredAnnot = s"${ L2_WrapFieldAccessesForIntegration.pIntAnnot }_$dim"
        L2_OffsetAllWithAnnotation.applyStandalone(upperResult)

        L2_OffsetAllApplicable.offset = getEffectiveOffset
        L2_OffsetAllApplicable.applyStandalone(lowerResult)
        L2_OffsetAllApplicable.applyStandalone(upperResult)

        result = lowerLength * lowerResult.expression + upperLength * upperResult.expression
      }
    }

    result
  }

  override def progress = ProgressLocation(L3_IntegrateOnGrid(name, level, expression.progress, L2_ProgressOption(offset)(_.progress)))
}

/// L2_ResolveIntegrateOnGrid

object L2_ResolveIntegrateOnGrid extends DefaultStrategy("Resolve grid integrations") {
  def isDoneIgnoreEval(expr : L2_Expression) = {
    StateManager.findFirst({ n : L2_MayBlockResolution =>
      n match {
        case _ : L2_EvaluateOnGrid            => false
        case mayBlock : L2_MayBlockResolution => !mayBlock.allDone
      }
    }, expr).isEmpty
  }

  this += new Transformation("Resolve", {
    case integrate : L2_IntegrateOnGrid if isDoneIgnoreEval(integrate) => integrate.resolve()
  })
}
