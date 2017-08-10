package exastencils.grid.l2

import scala.collection.mutable._

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
    if (offset.isDefined) out << offset
    out << " ( " << expression << " ) "
  }

  override def offsetWith(newOffset : L2_ConstIndex) = {
    if (offset.isEmpty)
      offset = Some(newOffset)
    else
      offset = Some(offset.get + newOffset)
  }

  def getEffectiveOffset = {
    var effectiveOffset = offset.getOrElse(L2_ConstIndex(Array.fill(numDims)(0)))

    // take into account that right-hand interfaces can be targets too
    val faceOffset = L2_GridUtil.offsetForFace(name.replace("integrateOver", ""))
    if (0 != faceOffset) effectiveOffset = L2_GridUtil.offsetIndex(effectiveOffset, faceOffset, faceDim)

    effectiveOffset
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

    def area = (0 until /*FIXME*/ Knowledge.dimensionality).filter(_ != faceDim).map(
      dim => cellInterfaceFor(dim) : L2_Expression).reduce(_ * _)

    // define offset expression
    def offsetExp = {
      val wrapped = L2_ExpressionStatement(Duplicate(exp))
      L2_OffsetAllApplicable.offset = getEffectiveOffset
      L2_OffsetAllApplicable.applyStandalone(wrapped)
      wrapped.expression
    }

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

              case L2_AtFaceCenter(fd) =>  // interpolation, piecewiseIntegration
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
                addPIntAnnot(curStagDim, L2_EvaluateOnGrid(stagDim, faceDim, level, L2_GridUtil.offsetAccess(fieldAccess, -1, curStagDim)))

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
          eval /* TODO
          if (eval.faceDim != faceDim) Logger.error(s"Found unaligned eval for faceDim ${ eval.faceDim } in integration for faceDim $faceDim in eval for ${ eval.fieldAccess }")
          if (eval.stagDim != stagDim) Logger.error(s"Found unaligned eval for stagDim ${ eval.stagDim } in integration for stagDim $stagDim in eval for ${ eval.fieldAccess }")

          val discr = eval.fieldAccess.fieldSelection.field.localization
          if (stagDim.isDefined) {
            val curStagDim = stagDim.get
            discr match {
              case L2_AtCellCenter if curStagDim == faceDim => eval.fieldAccess // direct sampling
              case L2_AtCellCenter if curStagDim != faceDim =>
                eval.fieldAccess = L2_GridUtil.offsetAccess(eval.fieldAccess, -1, curStagDim)
                addPIntAnnot(eval) // interpolation with offset, piecewiseIntegration
              case L2_AtFaceCenter(`curStagDim`)            => // field localization matches CV
                eval // orig eval is fine
              case L2_AtFaceCenter(fd)                      => // -1 in CV's stag dim and +1 in field localization's stag dim => ignore eval as direct sampling is possible
                addPIntAnnot(L2_GridUtil.offsetAccess(L2_GridUtil.offsetAccess(eval.fieldAccess, -1, curStagDim), 1, fd)) // direct sampling with offset, piecewiseIntegration
              case _                                        => Logger.error(s"Unknown or unsupported localization $discr for field ${ eval.fieldAccess } in staggered integration")
            }
          } else {
            discr match {
              case L2_AtCellCenter            => eval // interpolation
              case L2_AtFaceCenter(`faceDim`) =>
                eval.fieldAccess = L2_GridUtil.offsetAccess(eval.fieldAccess, 1, faceDim)
                eval // direct sampling with offset
              case L2_AtFaceCenter(_)         =>
                addPIntAnnot(eval) // interpolation, piecewiseIntegration
              case _                          => Logger.error(s"Unknown or unsupported localization $discr for field ${ eval.fieldAccess } in basic integration")
            }
          }*/

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
          case None | Some(`dim`) =>
            (0.5 * L2_VF_CellWidthPerDim.access(level, dim, Duplicate(index)),
              0.5 * L2_VF_CellWidthPerDim.access(level, dim, L2_GridUtil.offsetIndex(index, 1, dim)))
          case Some(_)            =>
            (0.5 * L2_VF_CellWidthPerDim.access(level, dim, L2_GridUtil.offsetIndex(index, -1, dim)),
              0.5 * L2_VF_CellWidthPerDim.access(level, dim, Duplicate(index)))
        }

        val lowerResult = L2_ExpressionStatement(result)
        val upperResult = L2_ExpressionStatement(Duplicate(result))
        L2_OffsetAllWithAnnotation.offset = L2_GridUtil.offsetIndex(L2_ConstIndex(Array.fill(numDims)(0)), 1, dim)
        L2_OffsetAllWithAnnotation.requiredAnnot = s"${ L2_WrapFieldAccessesForIntegration.pIntAnnot }_$dim"
        L2_OffsetAllWithAnnotation.applyStandalone(upperResult)

        result = lowerLength * lowerResult.expression + upperLength * upperResult.expression
      }
    }

    result
  }

  override def progress = L3_IntegrateOnGrid(name, level, expression.progress, L2_ProgressOption(offset)(_.progress))
}

/// L2_ResolveIntegrateOnGrid

object L2_ResolveIntegrateOnGrid extends DefaultStrategy("Resolve grid integrations") {
  this += new Transformation("Resolve", {
    case integrate : L2_IntegrateOnGrid if L2_MayBlockResolution.isDone(integrate) => integrate.resolve()
  })
}
