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

package exastencils.grid.l4

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_FieldIteratorAccess
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.datastructures._
import exastencils.fieldlike.l4.L4_FieldLikeAccess
import exastencils.grid.ir.IR_IntegrateOnGrid
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_CollectFieldAccesses

/// L4_IntegrateOnGrid

object L4_IntegrateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[L4_Expression], offset : Option[L4_ConstIndex]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      L4_RealConstant(0.0)
    } else {
      if (args.length > 1) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      new L4_IntegrateOnGrid(name, level, args(0), offset)
    }
  }
}

case class L4_IntegrateOnGrid(
    var name : String,
    var level : Int,
    var expression : L4_Expression,
    var offset : Option[L4_ConstIndex]) extends L4_Expression with L4_CanBeOffset with L4_MayBlockResolution {

  allDone = !Knowledge.experimental_l4_resolveVirtualFields

  def numDims = /*FIXME*/ Knowledge.dimensionality
  def stagDim = L4_GridUtil.faceToDims(name.replace("integrateOver", ""))._1
  def faceDim = L4_GridUtil.faceToDims(name.replace("integrateOver", ""))._2

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << offset
    out << " ( " << expression << " ) "
  }

  def getEffectiveOffset = {
    val effectiveOffset = offset.getOrElse(L4_ConstIndex(Array.fill(numDims)(0)))

    // take into account that right-hand interfaces can be targets too
    val faceOffset = L4_GridUtil.offsetForFace(name.replace("integrateOver", ""))
    L4_GridUtil.offsetIndex(effectiveOffset, faceOffset, faceDim)
  }

  def doIntegrate(exp : L4_Expression) = {
    var index = L4_FieldIteratorAccess.fullIndex(numDims)
    index += getEffectiveOffset

    // calculate size of integration domain (area)
    def cellInterfaceFor(dim : Int) =
      if (stagDim.isEmpty)  // integrate over regular cell interface
        L4_VF_CellWidthPerDim.access(level, dim, Duplicate(index))
      else // integrate over staggered cell interface
        L4_VF_StagCellWidthPerDim.access(level, stagDim.get, dim, Duplicate(index))

    def area = (0 until numDims).filter(_ != faceDim).map(dim => cellInterfaceFor(dim) : L4_Expression).reduce(_ * _)

    // define offset expression
    def offsetExp = {
      val wrapped = L4_ExpressionStatement(Duplicate(exp))
      L4_OffsetAllApplicable.offset = getEffectiveOffset
      L4_OffsetAllApplicable.applyStandalone(wrapped)
      wrapped.expression
    }

    // TODO: shift area?
    area * offsetExp
  }

  def resolve() : L4_Expression = {
    if (!Knowledge.grid_isAxisAligned) Logger.error("Non axis-aligned grids are currently not supported for integration")

    val wrapped = L4_ExpressionStatement(expression) // for matching purposes

    // collect (virtual) field accesses
    L4_CollectFieldAccesses.applyStandalone(wrapped)

    // check if all occurring level specifications are identical
    if (L4_CollectFieldAccesses.fieldAccesses.map(_.level).exists(_ != level)
      || L4_CollectFieldAccesses.vFieldAccesses.map(_.level).exists(_ != level))
      Logger.error(s"Mixed level integration is currently not supported (${ expression.prettyprint })")

    // if there are no field accesses direct integration is possible
    if (0 == L4_CollectFieldAccesses.fieldAccesses.length)
      return doIntegrate(expression)

    /// step 1: wrap field accesses with eval functions and mark them for piecewise integration if necessary

    object L4_WrapFieldAccessesForIntegration extends QuietDefaultStrategy("Wrap field accesses with evaluation nodes") {
      val pIntAnnot = "PIECEWISE_INTEGRATION" // +0 and +1 in the given dimension for piecewise integrations
      def addPIntAnnot(dim : Int, exp : L4_Expression) = { exp.annotate(s"${ pIntAnnot }_$dim"); exp }

      this += new Transformation("Wrap", {
        case fieldAccess : L4_FieldLikeAccess =>
          if (stagDim.isEmpty) { // non-staggered cells
            fieldAccess.target.localization match {
              case L4_AtCellCenter => // interpolation
                L4_EvaluateOnGrid(stagDim, faceDim, level, fieldAccess)

              case L4_AtFaceCenter(fd) if faceDim == fd => // direct sampling
                fieldAccess

              case L4_AtFaceCenter(fd) => // interpolation, piecewiseIntegration
                addPIntAnnot(fd, L4_EvaluateOnGrid(stagDim, faceDim, level, fieldAccess))

              case other => Logger.error(s"Unknown or unsupported localization $other for field $fieldAccess in basic integration")
            }
          } else {
            // staggered cells
            val curStagDim = stagDim.get
            fieldAccess.target.localization match {
              case L4_AtCellCenter if curStagDim == faceDim => // direct sampling with offset
                L4_GridUtil.offsetAccess(fieldAccess, -1, curStagDim)

              case L4_AtCellCenter if curStagDim != faceDim => // interpolation with offset, piecewiseIntegration
                addPIntAnnot(curStagDim, L4_EvaluateOnGrid(None, faceDim, level, L4_GridUtil.offsetAccess(fieldAccess, -1, curStagDim)))

              case L4_AtFaceCenter(`curStagDim`) => // field localization matches CV -> interpolation
                L4_EvaluateOnGrid(stagDim, faceDim, level, fieldAccess)

              case L4_AtFaceCenter(fd) if faceDim == curStagDim => // direct sampling with offset, piecewiseIntegration
                addPIntAnnot(fd, L4_GridUtil.offsetAccess(fieldAccess, -1, curStagDim))

              case L4_AtFaceCenter(fd) if faceDim == fd => // direct sampling with offset, piecewiseIntegration
                addPIntAnnot(curStagDim, L4_GridUtil.offsetAccess(fieldAccess, -1, curStagDim))

              case L4_AtFaceCenter(fd) => // interpolation with offset, double piecewiseIntegration
                addPIntAnnot(curStagDim, addPIntAnnot(fd,
                  L4_EvaluateOnGrid(stagDim, faceDim, level, L4_GridUtil.offsetAccess(fieldAccess, -1, curStagDim))))

              case other => Logger.error(s"Unknown or unsupported localization $other for field $fieldAccess in staggered integration")
            }
          }

        case fieldAccess : L4_VirtualFieldAccess =>
          Logger.error(s"Virtual field accesses ($fieldAccess) are currently unsupported within evaluation and integration functions")

        case eval : L4_EvaluateOnGrid =>
          if (eval.faceDim != faceDim) Logger.error(s"Found unaligned eval for faceDim ${ eval.faceDim } in integration for faceDim $faceDim in eval for ${ eval.expression }")
          if (eval.stagDim != stagDim) Logger.error(s"Found unaligned eval for stagDim ${ eval.stagDim } in integration for stagDim $stagDim in eval for ${ eval.expression }")

          // prevent double shift; occurs in, e.g., integrateRight ( evalRight ( exp ) )
          val evalOffset = L4_GridUtil.offsetForFace(eval.name.replace("evalAt", ""))
          val integrateOffset = L4_GridUtil.offsetForFace(L4_IntegrateOnGrid.this.name.replace("integrateOver", ""))
          if (evalOffset != integrateOffset) Logger.error("Nested evaluate and integrate face don't match")
          if (evalOffset != 0) eval.offsetWith(L4_GridUtil.offsetIndex(L4_ConstIndex(Array.fill(eval.numDims)(0)), -evalOffset, eval.faceDim))

          if (stagDim.isEmpty) {
            eval.fieldAccess().target.localization match {
              case L4_AtCellCenter => // interpolation (already sufficient)
                eval

              case L4_AtFaceCenter(fd) if faceDim == fd => // direct sampling
                eval.expression

              case L4_AtFaceCenter(fd) => // interpolation, piecewiseIntegration
                addPIntAnnot(fd, eval)

              case other => Logger.error(s"Unknown or unsupported localization $other for field ${ eval.fieldAccess() } in basic integration")
            }
          } else {
            val curStagDim = stagDim.get
            eval.fieldAccess().target.localization match {
              case L4_AtCellCenter if curStagDim == faceDim => // direct sampling with offset
                L4_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)

              case L4_AtCellCenter if curStagDim != faceDim => // interpolation with offset, piecewiseIntegration
                eval.name = s"evalAt${ L4_GridUtil.dimsToFace(None, faceDim) }" // evaluate at un-staggered cell interface
                eval.expression = L4_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                eval.expression = L4_GridUtil.offsetAccess(eval.fieldAccess(), 1, faceDim)
                addPIntAnnot(curStagDim, eval)

              case L4_AtFaceCenter(`curStagDim`) => // field localization matches CV -> interpolation
                eval

              case L4_AtFaceCenter(fd) if faceDim == curStagDim => // direct sampling with offset, piecewiseIntegration
                eval.expression = L4_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                addPIntAnnot(fd, eval)

              case L4_AtFaceCenter(fd) if faceDim == fd => // direct sampling with offset, piecewiseIntegration
                eval.expression = L4_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                addPIntAnnot(curStagDim, eval)

              case L4_AtFaceCenter(fd) => // interpolation with offset, double piecewiseIntegration
                eval.expression = L4_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                addPIntAnnot(curStagDim, addPIntAnnot(fd, eval))

              case other => Logger.error(s"Unknown or unsupported localization $other for field ${ eval.fieldAccess() } in staggered integration")
            }
          }

        case _ : L4_IntegrateOnGrid =>
          Logger.error("Integration functions called inside other integration functions are currently not supported")
      }, false) // not recursive -> don't look inside eval functions
    }
    L4_WrapFieldAccessesForIntegration.applyStandalone(wrapped)

    /// step 2: check if integration by parts is required

    val piecewiseIntegrationPerDim = (0 until numDims).map(dim => StateManager.findFirst({ n : Node =>
      n.annotations.contains(s"${ L4_WrapFieldAccessesForIntegration.pIntAnnot }_$dim")
    }, wrapped).isDefined)

    if (!piecewiseIntegrationPerDim.contains(true))
      return doIntegrate(wrapped.expression)

    /// step 3: apply chosen integration

    var index = L4_FieldIteratorAccess.fullIndex(numDims)
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
            L4_VF_CellWidthPerDim.access(level, dim, Duplicate(index))
          else // integrate over staggered cell interface
            L4_VF_StagCellWidthPerDim.access(level, stagDim.get, dim, Duplicate(index))

        result = length * result
      } else {
        // piecewise integration is required for the current dimension

        val (lowerLength, upperLength) = stagDim match {
          case None        => // 'normal' cell -> piecewise integration across lower and upper half of the cell
            (0.5 * L4_VF_CellWidthPerDim.access(level, dim, Duplicate(index)),
              0.5 * L4_VF_CellWidthPerDim.access(level, dim, Duplicate(index)))
          case Some(`dim`) => // integration in the direction of the staggering -> half of the left cell and half of the current cell
            (0.5 * L4_VF_CellWidthPerDim.access(level, dim, L4_GridUtil.offsetIndex(index, -1, dim)),
              0.5 * L4_VF_CellWidthPerDim.access(level, dim, Duplicate(index)))
          case Some(_)     => // integration on a staggered cell, but not in the direction of the staggering -> handle like an unstaggered cell
            (0.5 * L4_VF_CellWidthPerDim.access(level, dim, Duplicate(index)),
              0.5 * L4_VF_CellWidthPerDim.access(level, dim, Duplicate(index)))
        }

        val lowerResult = L4_ExpressionStatement(result)
        val upperResult = L4_ExpressionStatement(Duplicate(result))
        L4_OffsetAllWithAnnotation.offset = L4_GridUtil.offsetIndex(L4_ConstIndex(Array.fill(numDims)(0)), 1, dim)
        L4_OffsetAllWithAnnotation.requiredAnnot = s"${ L4_WrapFieldAccessesForIntegration.pIntAnnot }_$dim"
        L4_OffsetAllWithAnnotation.applyStandalone(upperResult)

        L4_OffsetAllApplicable.offset = getEffectiveOffset
        L4_OffsetAllApplicable.applyStandalone(lowerResult)
        L4_OffsetAllApplicable.applyStandalone(upperResult)

        result = lowerLength * lowerResult.expression + upperLength * upperResult.expression
      }
    }

    result
  }

  override def progress = ProgressLocation(IR_IntegrateOnGrid(name, level, expression.progress, L4_ProgressOption(offset)(_.progress)))
}

/// L4_ResolveIntegrateOnGrid

object L4_ResolveIntegrateOnGrid extends DefaultStrategy("Resolve grid integrations") {
  def isDoneIgnoreEval(expr : L4_Expression) = {
    StateManager.findFirst({ n : L4_MayBlockResolution =>
      n match {
        case _ : L4_EvaluateOnGrid            => false
        case mayBlock : L4_MayBlockResolution => !mayBlock.allDone
      }
    }, expr).isEmpty
  }

  this += new Transformation("Resolve", {
    case integrate : L4_IntegrateOnGrid if isDoneIgnoreEval(integrate) => integrate.resolve()
  })
}
