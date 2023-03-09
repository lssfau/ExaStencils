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

package exastencils.grid.l3

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_FieldIteratorAccess
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.datastructures._
import exastencils.field.l3.L3_FieldAccess
import exastencils.grid.l4.L4_IntegrateOnGrid
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l3.L3_CollectFieldAccesses

/// L3_IntegrateOnGrid

object L3_IntegrateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[L3_Expression], offset : Option[L3_ConstIndex]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      L3_RealConstant(0.0)
    } else {
      if (args.length > 1) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      new L3_IntegrateOnGrid(name, level, args(0), offset)
    }
  }
}

case class L3_IntegrateOnGrid(
    var name : String,
    var level : Int,
    var expression : L3_Expression,
    var offset : Option[L3_ConstIndex]) extends L3_Expression with L3_CanBeOffset with L3_MayBlockResolution {

  allDone = !Knowledge.experimental_l3_resolveVirtualFields

  def numDims = /*FIXME*/ Knowledge.dimensionality
  def stagDim = L3_GridUtil.faceToDims(name.replace("integrateOver", ""))._1
  def faceDim = L3_GridUtil.faceToDims(name.replace("integrateOver", ""))._2

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << offset.get
    out << " ( " << expression << " ) "
  }

  def getEffectiveOffset = {
    val effectiveOffset = offset.getOrElse(L3_ConstIndex(Array.fill(numDims)(0)))

    // take into account that right-hand interfaces can be targets too
    val faceOffset = L3_GridUtil.offsetForFace(name.replace("integrateOver", ""))
    L3_GridUtil.offsetIndex(effectiveOffset, faceOffset, faceDim)
  }

  def doIntegrate(exp : L3_Expression) = {
    var index = L3_FieldIteratorAccess.fullIndex(numDims)
    index += getEffectiveOffset

    // calculate size of integration domain (area)
    def cellInterfaceFor(dim : Int) =
      if (stagDim.isEmpty)  // integrate over regular cell interface
        L3_VF_CellWidthPerDim.access(level, dim, Duplicate(index))
      else // integrate over staggered cell interface
        L3_VF_StagCellWidthPerDim.access(level, stagDim.get, dim, Duplicate(index))

    def area = (0 until numDims).filter(_ != faceDim).map(dim => cellInterfaceFor(dim) : L3_Expression).reduce(_ * _)

    // define offset expression
    def offsetExp = {
      val wrapped = L3_ExpressionStatement(Duplicate(exp))
      L3_OffsetAllApplicable.offset = getEffectiveOffset
      L3_OffsetAllApplicable.applyStandalone(wrapped)
      wrapped.expression
    }

    // TODO: shift area?
    area * offsetExp
  }

  def resolve() : L3_Expression = {
    if (!Knowledge.grid_isAxisAligned) Logger.error("Non axis-aligned grids are currently not supported for integration")

    val wrapped = L3_ExpressionStatement(expression) // for matching purposes

    // collect (virtual) field accesses
    L3_CollectFieldAccesses.applyStandalone(wrapped)

    // check if all occurring level specifications are identical
    if (L3_CollectFieldAccesses.fieldAccesses.map(_.level).exists(_ != level)
      || L3_CollectFieldAccesses.vFieldAccesses.map(_.level).exists(_ != level))
      Logger.error(s"Mixed level integration is currently not supported (${ expression.prettyprint })")

    // if there are no field accesses direct integration is possible
    if (0 == L3_CollectFieldAccesses.fieldAccesses.length)
      return doIntegrate(expression)

    /// step 1: wrap field accesses with eval functions and mark them for piecewise integration if necessary

    object L3_WrapFieldAccessesForIntegration extends QuietDefaultStrategy("Wrap field accesses with evaluation nodes") {
      val pIntAnnot = "PIECEWISE_INTEGRATION" // +0 and +1 in the given dimension for piecewise integrations
      def addPIntAnnot(dim : Int, exp : L3_Expression) = { exp.annotate(s"${ pIntAnnot }_$dim"); exp }

      this += new Transformation("Wrap", {
        case fieldAccess : L3_FieldAccess =>
          if (stagDim.isEmpty) { // non-staggered cells
            fieldAccess.target.localization match {
              case L3_AtCellCenter => // interpolation
                L3_EvaluateOnGrid(stagDim, faceDim, level, fieldAccess)

              case L3_AtFaceCenter(fd) if faceDim == fd => // direct sampling
                fieldAccess

              case L3_AtFaceCenter(fd) => // interpolation, piecewiseIntegration
                addPIntAnnot(fd, L3_EvaluateOnGrid(stagDim, faceDim, level, fieldAccess))

              case other => Logger.error(s"Unknown or unsupported localization $other for field $fieldAccess in basic integration")
            }
          } else {
            // staggered cells
            val curStagDim = stagDim.get
            fieldAccess.target.localization match {
              case L3_AtCellCenter if curStagDim == faceDim => // direct sampling with offset
                L3_GridUtil.offsetAccess(fieldAccess, -1, curStagDim)

              case L3_AtCellCenter if curStagDim != faceDim => // interpolation with offset, piecewiseIntegration
                addPIntAnnot(curStagDim, L3_EvaluateOnGrid(None, faceDim, level, L3_GridUtil.offsetAccess(fieldAccess, -1, curStagDim)))

              case L3_AtFaceCenter(`curStagDim`) => // field localization matches CV -> interpolation
                L3_EvaluateOnGrid(stagDim, faceDim, level, fieldAccess)

              case L3_AtFaceCenter(fd) if faceDim == curStagDim => // direct sampling with offset, piecewiseIntegration
                addPIntAnnot(fd, L3_GridUtil.offsetAccess(fieldAccess, -1, curStagDim))

              case L3_AtFaceCenter(fd) if faceDim == fd => // direct sampling with offset, piecewiseIntegration
                addPIntAnnot(curStagDim, L3_GridUtil.offsetAccess(fieldAccess, -1, curStagDim))

              case L3_AtFaceCenter(fd) => // interpolation with offset, double piecewiseIntegration
                addPIntAnnot(curStagDim, addPIntAnnot(fd,
                  L3_EvaluateOnGrid(stagDim, faceDim, level, L3_GridUtil.offsetAccess(fieldAccess, -1, curStagDim))))

              case other => Logger.error(s"Unknown or unsupported localization $other for field $fieldAccess in staggered integration")
            }
          }

        case fieldAccess : L3_VirtualFieldAccess =>
          Logger.error(s"Virtual field accesses ($fieldAccess) are currently unsupported within evaluation and integration functions")

        case eval : L3_EvaluateOnGrid =>
          if (eval.faceDim != faceDim) Logger.error(s"Found unaligned eval for faceDim ${ eval.faceDim } in integration for faceDim $faceDim in eval for ${ eval.expression }")
          if (eval.stagDim != stagDim) Logger.error(s"Found unaligned eval for stagDim ${ eval.stagDim } in integration for stagDim $stagDim in eval for ${ eval.expression }")

          // prevent double shift; occurs in, e.g., integrateRight ( evalRight ( exp ) )
          val evalOffset = L3_GridUtil.offsetForFace(eval.name.replace("evalAt", ""))
          val integrateOffset = L3_GridUtil.offsetForFace(L3_IntegrateOnGrid.this.name.replace("integrateOver", ""))
          if (evalOffset != integrateOffset) Logger.error("Nested evaluate and integrate face don't match")
          if (evalOffset != 0) eval.offsetWith(L3_GridUtil.offsetIndex(L3_ConstIndex(Array.fill(eval.numDims)(0)), -evalOffset, eval.faceDim))

          if (stagDim.isEmpty) {
            eval.fieldAccess().target.localization match {
              case L3_AtCellCenter => // interpolation (already sufficient)
                eval

              case L3_AtFaceCenter(fd) if faceDim == fd => // direct sampling
                eval.expression

              case L3_AtFaceCenter(fd) => // interpolation, piecewiseIntegration
                addPIntAnnot(fd, eval)

              case other => Logger.error(s"Unknown or unsupported localization $other for field ${ eval.fieldAccess() } in basic integration")
            }
          } else {
            val curStagDim = stagDim.get
            eval.fieldAccess().target.localization match {
              case L3_AtCellCenter if curStagDim == faceDim => // direct sampling with offset
                L3_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)

              case L3_AtCellCenter if curStagDim != faceDim => // interpolation with offset, piecewiseIntegration
                eval.name = s"evalAt${ L3_GridUtil.dimsToFace(None, faceDim) }" // evaluate at un-staggered cell interface
                eval.expression = L3_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                eval.expression = L3_GridUtil.offsetAccess(eval.fieldAccess(), 1, faceDim)
                addPIntAnnot(curStagDim, eval)

              case L3_AtFaceCenter(`curStagDim`) => // field localization matches CV -> interpolation
                eval

              case L3_AtFaceCenter(fd) if faceDim == curStagDim => // direct sampling with offset, piecewiseIntegration
                eval.expression = L3_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                addPIntAnnot(fd, eval)

              case L3_AtFaceCenter(fd) if faceDim == fd => // direct sampling with offset, piecewiseIntegration
                eval.expression = L3_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                addPIntAnnot(curStagDim, eval)

              case L3_AtFaceCenter(fd) => // interpolation with offset, double piecewiseIntegration
                eval.expression = L3_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                addPIntAnnot(curStagDim, addPIntAnnot(fd, eval))

              case other => Logger.error(s"Unknown or unsupported localization $other for field ${ eval.fieldAccess() } in staggered integration")
            }
          }

        case _ : L3_IntegrateOnGrid =>
          Logger.error("Integration functions called inside other integration functions are currently not supported")
      }, false) // not recursive -> don't look inside eval functions
    }
    L3_WrapFieldAccessesForIntegration.applyStandalone(wrapped)

    /// step 2: check if integration by parts is required

    val piecewiseIntegrationPerDim = (0 until numDims).map(dim => StateManager.findFirst({ n : Node =>
      n.annotations.contains(s"${ L3_WrapFieldAccessesForIntegration.pIntAnnot }_$dim")
    }, wrapped).isDefined)

    if (!piecewiseIntegrationPerDim.contains(true))
      return doIntegrate(wrapped.expression)

    /// step 3: apply chosen integration

    var index = L3_FieldIteratorAccess.fullIndex(numDims)
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
            L3_VF_CellWidthPerDim.access(level, dim, Duplicate(index))
          else // integrate over staggered cell interface
            L3_VF_StagCellWidthPerDim.access(level, stagDim.get, dim, Duplicate(index))

        result = length * result
      } else {
        // piecewise integration is required for the current dimension

        val (lowerLength, upperLength) = stagDim match {
          case None        => // 'normal' cell -> piecewise integration across lower and upper half of the cell
            (0.5 * L3_VF_CellWidthPerDim.access(level, dim, Duplicate(index)),
              0.5 * L3_VF_CellWidthPerDim.access(level, dim, Duplicate(index)))
          case Some(`dim`) => // integration in the direction of the staggering -> half of the left cell and half of the current cell
            (0.5 * L3_VF_CellWidthPerDim.access(level, dim, L3_GridUtil.offsetIndex(index, -1, dim)),
              0.5 * L3_VF_CellWidthPerDim.access(level, dim, Duplicate(index)))
          case Some(_)     => // integration on a staggered cell, but not in the direction of the staggering -> handle like an unstaggered cell
            (0.5 * L3_VF_CellWidthPerDim.access(level, dim, Duplicate(index)),
              0.5 * L3_VF_CellWidthPerDim.access(level, dim, Duplicate(index)))
        }

        val lowerResult = L3_ExpressionStatement(result)
        val upperResult = L3_ExpressionStatement(Duplicate(result))
        L3_OffsetAllWithAnnotation.offset = L3_GridUtil.offsetIndex(L3_ConstIndex(Array.fill(numDims)(0)), 1, dim)
        L3_OffsetAllWithAnnotation.requiredAnnot = s"${ L3_WrapFieldAccessesForIntegration.pIntAnnot }_$dim"
        L3_OffsetAllWithAnnotation.applyStandalone(upperResult)

        L3_OffsetAllApplicable.offset = getEffectiveOffset
        L3_OffsetAllApplicable.applyStandalone(lowerResult)
        L3_OffsetAllApplicable.applyStandalone(upperResult)

        result = lowerLength * lowerResult.expression + upperLength * upperResult.expression
      }
    }

    result
  }

  override def progress = ProgressLocation(L4_IntegrateOnGrid(name, level, expression.progress, L3_ProgressOption(offset)(_.progress)))
}

/// L3_ResolveIntegrateOnGrid

object L3_ResolveIntegrateOnGrid extends DefaultStrategy("Resolve grid integrations") {
  def isDoneIgnoreEval(expr : L3_Expression) = {
    StateManager.findFirst({ n : L3_MayBlockResolution =>
      n match {
        case _ : L3_EvaluateOnGrid            => false
        case mayBlock : L3_MayBlockResolution => !mayBlock.allDone
      }
    }, expr).isEmpty
  }

  this += new Transformation("Resolve", {
    case integrate : L3_IntegrateOnGrid if isDoneIgnoreEval(integrate) => integrate.resolve()
  })
}
