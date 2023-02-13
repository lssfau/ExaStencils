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

package exastencils.grid.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FieldIteratorAccess
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.datastructures._
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_CollectFieldAccesses

/// IR_IntegrateOnGrid

object IR_IntegrateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[IR_Expression], offset : Option[IR_ConstIndex]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      IR_RealConstant(0.0)
    } else {
      if (args.length > 1) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      new IR_IntegrateOnGrid(name, level, args(0), offset)
    }
  }
}

case class IR_IntegrateOnGrid(
    var name : String,
    var level : Int,
    var expression : IR_Expression,
    var offset : Option[IR_ConstIndex]) extends IR_Expression with IR_CanBeOffset {

  override def datatype = expression.datatype

  def numDims = /*FIXME*/ Knowledge.dimensionality
  def stagDim = IR_GridUtil.faceToDims(name.replace("integrateOver", ""))._1
  def faceDim = IR_GridUtil.faceToDims(name.replace("integrateOver", ""))._2

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << offset
    out << " ( " << expression << " ) "
  }

  def getEffectiveOffset = {
    val effectiveOffset = offset.getOrElse(IR_ConstIndex(Array.fill(numDims)(0)))

    // take into account that right-hand interfaces can be targets too
    val faceOffset = IR_GridUtil.offsetForFace(name.replace("integrateOver", ""))
    IR_GridUtil.offsetIndex(effectiveOffset, faceOffset, faceDim)
  }

  def doIntegrate(exp : IR_Expression) = {
    var index = IR_FieldIteratorAccess.fullIndex(numDims)
    index += getEffectiveOffset

    // calculate size of integration domain (area)
    def cellInterfaceFor(dim : Int) =
      if (stagDim.isEmpty)  // integrate over regular cell interface
        IR_VF_CellWidthPerDim.access(level, dim, Duplicate(index))
      else // integrate over staggered cell interface
        IR_VF_StagCellWidthPerDim.access(level, stagDim.get, dim, Duplicate(index))

    def area = (0 until numDims).filter(_ != faceDim).map(dim => cellInterfaceFor(dim) : IR_Expression).reduce(_ * _)

    // define offset expression
    def offsetExp = {
      val wrapped = IR_ExpressionStatement(Duplicate(exp))
      IR_OffsetAllApplicable.offset = getEffectiveOffset
      IR_OffsetAllApplicable.applyStandalone(wrapped)
      wrapped.expression
    }

    // TODO: shift area?
    area * offsetExp
  }

  def resolve() : IR_Expression = {
    if (!Knowledge.grid_isAxisAligned) Logger.error("Non axis-aligned grids are currently not supported for integration")

    val wrapped = IR_ExpressionStatement(expression) // for matching purposes

    // collect (virtual) field accesses
    IR_CollectFieldAccesses.applyStandalone(wrapped)

    // check if all occurring level specifications are identical
    if (IR_CollectFieldAccesses.fieldAccesses.map(_.field.level).exists(_ != level)
      || IR_CollectFieldAccesses.vFieldAccesses.map(_.level).exists(_ != level))
      Logger.error(s"Mixed level integration is currently not supported (${ expression.prettyprint })")

    // if there are no field accesses direct integration is possible
    if (0 == IR_CollectFieldAccesses.fieldAccesses.length)
      return doIntegrate(expression)

    /// step 1: wrap field accesses with eval functions and mark them for piecewise integration if necessary

    object IR_WrapFieldAccessesForIntegration extends QuietDefaultStrategy("Wrap field accesses with evaluation nodes") {
      val pIntAnnot = "PIECEWISE_INTEGRATION" // +0 and +1 in the given dimension for piecewise integrations
      def addPIntAnnot(dim : Int, exp : IR_Expression) = { exp.annotate(s"${ pIntAnnot }_$dim"); exp }

      this += new Transformation("Wrap", {
        case fieldAccess : IR_FieldLikeAccess =>
          if (stagDim.isEmpty) { // non-staggered cells
            fieldAccess.field.localization match {
              case IR_AtCellCenter => // interpolation
                IR_EvaluateOnGrid(stagDim, faceDim, level, fieldAccess)

              case IR_AtFaceCenter(fd) if faceDim == fd => // direct sampling
                fieldAccess

              case IR_AtFaceCenter(fd) => // interpolation, piecewiseIntegration
                addPIntAnnot(fd, IR_EvaluateOnGrid(stagDim, faceDim, level, fieldAccess))

              case other => Logger.error(s"Unknown or unsupported localization $other for field $fieldAccess in basic integration")
            }
          } else {
            // staggered cells
            val curStagDim = stagDim.get
            fieldAccess.field.localization match {
              case IR_AtCellCenter if curStagDim == faceDim => // direct sampling with offset
                IR_GridUtil.offsetAccess(fieldAccess, -1, curStagDim)

              case IR_AtCellCenter if curStagDim != faceDim => // interpolation with offset, piecewiseIntegration
                addPIntAnnot(curStagDim, IR_EvaluateOnGrid(None, faceDim, level, IR_GridUtil.offsetAccess(fieldAccess, -1, curStagDim)))

              case IR_AtFaceCenter(`curStagDim`) => // field localization matches CV -> interpolation
                IR_EvaluateOnGrid(stagDim, faceDim, level, fieldAccess)

              case IR_AtFaceCenter(fd) if faceDim == curStagDim => // direct sampling with offset, piecewiseIntegration
                addPIntAnnot(fd, IR_GridUtil.offsetAccess(fieldAccess, -1, curStagDim))

              case IR_AtFaceCenter(fd) if faceDim == fd => // direct sampling with offset, piecewiseIntegration
                addPIntAnnot(curStagDim, IR_GridUtil.offsetAccess(fieldAccess, -1, curStagDim))

              case IR_AtFaceCenter(fd) => // interpolation with offset, double piecewiseIntegration
                addPIntAnnot(curStagDim, addPIntAnnot(fd,
                  IR_EvaluateOnGrid(stagDim, faceDim, level, IR_GridUtil.offsetAccess(fieldAccess, -1, curStagDim))))

              case other => Logger.error(s"Unknown or unsupported localization $other for field $fieldAccess in staggered integration")
            }
          }

        case fieldAccess : IR_VirtualFieldAccess =>
          Logger.error(s"Virtual field accesses ($fieldAccess) are currently unsupported within evaluation and integration functions")

        case eval : IR_EvaluateOnGrid =>
          if (eval.faceDim != faceDim) Logger.error(s"Found unaligned eval for faceDim ${ eval.faceDim } in integration for faceDim $faceDim in eval for ${ eval.expression }")
          if (eval.stagDim != stagDim) Logger.error(s"Found unaligned eval for stagDim ${ eval.stagDim } in integration for stagDim $stagDim in eval for ${ eval.expression }")

          // prevent double shift; occurs in, e.g., integrateRight ( evalRight ( exp ) )
          val evalOffset = IR_GridUtil.offsetForFace(eval.name.replace("evalAt", ""))
          val integrateOffset = IR_GridUtil.offsetForFace(IR_IntegrateOnGrid.this.name.replace("integrateOver", ""))
          if (evalOffset != integrateOffset) Logger.error("Nested evaluate and integrate face don't match")
          if (evalOffset != 0) eval.offsetWith(IR_GridUtil.offsetIndex(IR_ConstIndex(Array.fill(eval.numDims)(0)), -evalOffset, eval.faceDim))

          if (stagDim.isEmpty) {
            eval.fieldAccess().field.localization match {
              case IR_AtCellCenter => // interpolation (already sufficient)
                eval

              case IR_AtFaceCenter(fd) if faceDim == fd => // direct sampling
                eval.expression

              case IR_AtFaceCenter(fd) => // interpolation, piecewiseIntegration
                addPIntAnnot(fd, eval)

              case other => Logger.error(s"Unknown or unsupported localization $other for field ${ eval.fieldAccess() } in basic integration")
            }
          } else {
            val curStagDim = stagDim.get
            eval.fieldAccess().field.localization match {
              case IR_AtCellCenter if curStagDim == faceDim => // direct sampling with offset
                IR_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)

              case IR_AtCellCenter if curStagDim != faceDim => // interpolation with offset, piecewiseIntegration
                eval.name = s"evalAt${ IR_GridUtil.dimsToFace(None, faceDim) }" // evaluate at un-staggered cell interface
                eval.expression = IR_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                eval.expression = IR_GridUtil.offsetAccess(eval.fieldAccess(), 1, faceDim)
                addPIntAnnot(curStagDim, eval)

              case IR_AtFaceCenter(`curStagDim`) => // field localization matches CV -> interpolation
                eval

              case IR_AtFaceCenter(fd) if faceDim == curStagDim => // direct sampling with offset, piecewiseIntegration
                eval.expression = IR_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                addPIntAnnot(fd, eval)

              case IR_AtFaceCenter(fd) if faceDim == fd => // direct sampling with offset, piecewiseIntegration
                eval.expression = IR_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                addPIntAnnot(curStagDim, eval)

              case IR_AtFaceCenter(fd) => // interpolation with offset, double piecewiseIntegration
                eval.expression = IR_GridUtil.offsetAccess(eval.fieldAccess(), -1, curStagDim)
                addPIntAnnot(curStagDim, addPIntAnnot(fd, eval))

              case other => Logger.error(s"Unknown or unsupported localization $other for field ${ eval.fieldAccess() } in staggered integration")
            }
          }

        case _ : IR_IntegrateOnGrid =>
          Logger.error("Integration functions called inside other integration functions are currently not supported")
      }, false) // not recursive -> don't look inside eval functions
    }
    IR_WrapFieldAccessesForIntegration.applyStandalone(wrapped)

    /// step 2: check if integration by parts is required

    val piecewiseIntegrationPerDim = (0 until numDims).map(dim => StateManager.findFirst({ n : Node =>
      n.annotations.contains(s"${ IR_WrapFieldAccessesForIntegration.pIntAnnot }_$dim")
    }, wrapped).isDefined)

    if (!piecewiseIntegrationPerDim.contains(true))
      return doIntegrate(wrapped.expression)

    /// step 3: apply chosen integration

    var index = IR_FieldIteratorAccess.fullIndex(numDims)
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
            IR_VF_CellWidthPerDim.access(level, dim, Duplicate(index))
          else // integrate over staggered cell interface
            IR_VF_StagCellWidthPerDim.access(level, stagDim.get, dim, Duplicate(index))

        result = length * result
      } else {
        // piecewise integration is required for the current dimension

        val (lowerLength, upperLength) = stagDim match {
          case None        => // 'normal' cell -> piecewise integration across lower and upper half of the cell
            (0.5 * IR_VF_CellWidthPerDim.access(level, dim, Duplicate(index)),
              0.5 * IR_VF_CellWidthPerDim.access(level, dim, Duplicate(index)))
          case Some(`dim`) => // integration in the direction of the staggering -> half of the left cell and half of the current cell
            (0.5 * IR_VF_CellWidthPerDim.access(level, dim, IR_GridUtil.offsetIndex(index, -1, dim)),
              0.5 * IR_VF_CellWidthPerDim.access(level, dim, Duplicate(index)))
          case Some(_)     => // integration on a staggered cell, but not in the direction of the staggering -> handle like an unstaggered cell
            (0.5 * IR_VF_CellWidthPerDim.access(level, dim, Duplicate(index)),
              0.5 * IR_VF_CellWidthPerDim.access(level, dim, Duplicate(index)))
        }

        val lowerResult = IR_ExpressionStatement(result)
        val upperResult = IR_ExpressionStatement(Duplicate(result))
        IR_OffsetAllWithAnnotation.offset = IR_GridUtil.offsetIndex(IR_ConstIndex(Array.fill(numDims)(0)), 1, dim)
        IR_OffsetAllWithAnnotation.requiredAnnot = s"${ IR_WrapFieldAccessesForIntegration.pIntAnnot }_$dim"
        IR_OffsetAllWithAnnotation.applyStandalone(upperResult)

        IR_OffsetAllApplicable.offset = getEffectiveOffset
        IR_OffsetAllApplicable.applyStandalone(lowerResult)
        IR_OffsetAllApplicable.applyStandalone(upperResult)

        result = lowerLength * lowerResult.expression + upperLength * upperResult.expression
      }
    }

    result
  }
}

/// IR_ResolveIntegrateOnGrid

object IR_ResolveIntegrateOnGrid extends DefaultStrategy("Resolve grid integrations") {
  this += new Transformation("Resolve", {
    case integrate : IR_IntegrateOnGrid => integrate.resolve()
  })
}
