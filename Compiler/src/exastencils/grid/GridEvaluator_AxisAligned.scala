package exastencils.grid

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_DimToString
import exastencils.field.ir.IR_FieldAccess
import exastencils.grid.ir._
import exastencils.logger._

/// GridEvaluator_AxisAligned

object GridEvaluator_AxisAligned extends GridEvaluator {
  def geom = GridGeometry.getGeometry

  // evaluations and interpolations
  def evalAtEastFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    EvalAtRFace(fieldAccess, level, 0, None, interpolation, offset)
  def evalAtWestFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    evalAtLFace(fieldAccess, level, 0, None, interpolation, offset)
  def evalAtNorthFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    EvalAtRFace(fieldAccess, level, 1, None, interpolation, offset)
  def evalAtSouthFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    evalAtLFace(fieldAccess, level, 1, None, interpolation, offset)
  def evalAtTopFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    EvalAtRFace(fieldAccess, level, 2, None, interpolation, offset)
  def evalAtBottomFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    evalAtLFace(fieldAccess, level, 2, None, interpolation, offset)

  def evalAtXStaggeredEastFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    EvalAtRFace(fieldAccess, level, 0, Some(0), interpolation, offset)
  def evalAtXStaggeredWestFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    evalAtLFace(fieldAccess, level, 0, Some(0), interpolation, offset)
  def evalAtXStaggeredNorthFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    EvalAtRFace(fieldAccess, level, 1, Some(0), interpolation, offset)
  def evalAtXStaggeredSouthFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    evalAtLFace(fieldAccess, level, 1, Some(0), interpolation, offset)
  def evalAtXStaggeredTopFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    EvalAtRFace(fieldAccess, level, 2, Some(0), interpolation, offset)
  def evalAtXStaggeredBottomFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    evalAtLFace(fieldAccess, level, 2, Some(0), interpolation, offset)

  def evalAtYStaggeredEastFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    EvalAtRFace(fieldAccess, level, 0, Some(1), interpolation, offset)
  def evalAtYStaggeredWestFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    evalAtLFace(fieldAccess, level, 0, Some(1), interpolation, offset)
  def evalAtYStaggeredNorthFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    EvalAtRFace(fieldAccess, level, 1, Some(1), interpolation, offset)
  def evalAtYStaggeredSouthFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    evalAtLFace(fieldAccess, level, 1, Some(1), interpolation, offset)
  def evalAtYStaggeredTopFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    EvalAtRFace(fieldAccess, level, 2, Some(1), interpolation, offset)
  def evalAtYStaggeredBottomFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    evalAtLFace(fieldAccess, level, 2, Some(1), interpolation, offset)

  def evalAtZStaggeredEastFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    EvalAtRFace(fieldAccess, level, 0, Some(2), interpolation, offset)
  def evalAtZStaggeredWestFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    evalAtLFace(fieldAccess, level, 0, Some(2), interpolation, offset)
  def evalAtZStaggeredNorthFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    EvalAtRFace(fieldAccess, level, 1, Some(2), interpolation, offset)
  def evalAtZStaggeredSouthFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    evalAtLFace(fieldAccess, level, 1, Some(2), interpolation, offset)
  def evalAtZStaggeredTopFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    EvalAtRFace(fieldAccess, level, 2, Some(2), interpolation, offset)
  def evalAtZStaggeredBottomFace(fieldAccess : IR_FieldAccess, level : Int, interpolation : String, offset : Option[IR_ConstIndex]) =
    evalAtLFace(fieldAccess, level, 2, Some(2), interpolation, offset)

  def evalAtLFace(fieldAccess : IR_FieldAccess, level : Int, faceDim : Int, stagDim : Option[Int], interpolation : String, offset : Option[IR_ConstIndex]) =
    EvalAtRFace(GridUtil.offsetAccess(fieldAccess, -1, faceDim), level, faceDim, stagDim, interpolation, offset)

  case class EvalAtRFace(
      var fieldAccess : IR_FieldAccess,
      var level : Int,
      var faceDim : Int,
      var stagDim : Option[Int],
      var interpolation : String = "default",
      var offset : Option[IR_ConstIndex] = None) extends IR_Expression with IR_SpecialExpandable {

    override def datatype = IR_UnitDatatype

    def expandSpecial : Output[IR_Expression] = {
      val field = fieldAccess.fieldSelection.field
      val level = field.level

      var baseIndex = fieldAccess.index
      if (offset.isDefined)
        baseIndex += offset.get

      var a0 : (() => IR_Expression) = () => { IR_NullExpression }
      var a1 : (() => IR_Expression) = () => { IR_NullExpression }
      def x0 = Duplicate(fieldAccess)
      def x1 = GridUtil.offsetAccess(fieldAccess, 1, faceDim)

      field.discretization match {
        case "cell" =>
          if (stagDim.isDefined) {
            return fieldAccess // value is located at the evaluation region
          } else {
            a0 = () => { geom.cellCenterToFace(level, Duplicate(baseIndex), None, faceDim) }
            a1 = () => { geom.cellCenterToFace(level, GridUtil.offsetIndex(baseIndex, 1, faceDim), None, faceDim) }
          }

        case "face_x" | "face_y" | "face_z" =>
          if (stagDim.isDefined) {
            if (s"face_${ IR_DimToString(faceDim) }" == field.discretization) {
              // interpolation weights are always 0.5 due to geometric construction
              a0 = () => { 0.5 }
              a1 = () => { 0.5 }
            } else {
              a0 = () => { geom.cellCenterToFace(level, Duplicate(baseIndex), None, faceDim) }
              a1 = () => { geom.cellCenterToFace(level, GridUtil.offsetIndex(baseIndex, 1, faceDim), None, faceDim) }

              //Logger.warn(s"Trying to evaluate $fieldAccess on face dimension $faceDim of a ${ stagDim.get } staggered CV. This is not unique. Defaulting to leftmost candidate")
              //return GridUtil.offsetAccess(fieldAccess, 1, stagDim.get)
            }
          } else {
            return fieldAccess // value is located at the evaluation region
          }
      }

      // compile evaluation
      interpolation match {
        case "linear" | "default" => (a1() * x0 + a0() * x1) / (a0() + a1())
        case "harmonicMean"       => ((a0() + a1()) * (x0 * x1)) / (a1() * x0 + a0() * x1)
        case _                    =>
          Logger.warn(s"Trying to use interpolation scheme $interpolation which is unknown - falling back to default scheme")
          EvalAtRFace(fieldAccess, level, faceDim, stagDim, "default", offset).expandSpecial
      }
    }
  }

  // integrations
  def integrateOverEastFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverRFace(exp, level, 0, None, offset)
  def integrateOverWestFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverLFace(exp, level, 0, None, offset)
  def integrateOverNorthFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverRFace(exp, level, 1, None, offset)
  def integrateOverSouthFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverLFace(exp, level, 1, None, offset)
  def integrateOverTopFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverRFace(exp, level, 2, None, offset)
  def integrateOverBottomFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverLFace(exp, level, 2, None, offset)

  def integrateOverXStaggeredEastFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverRFace(exp, level, 0, Some(0), offset)
  def integrateOverXStaggeredWestFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverLFace(exp, level, 0, Some(0), offset)
  def integrateOverXStaggeredNorthFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverRFace(exp, level, 1, Some(0), offset)
  def integrateOverXStaggeredSouthFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverLFace(exp, level, 1, Some(0), offset)
  def integrateOverXStaggeredTopFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverRFace(exp, level, 2, Some(0), offset)
  def integrateOverXStaggeredBottomFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverLFace(exp, level, 2, Some(0), offset)

  def integrateOverYStaggeredEastFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverRFace(exp, level, 0, Some(1), offset)
  def integrateOverYStaggeredWestFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverLFace(exp, level, 0, Some(1), offset)
  def integrateOverYStaggeredNorthFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverRFace(exp, level, 1, Some(1), offset)
  def integrateOverYStaggeredSouthFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverLFace(exp, level, 1, Some(1), offset)
  def integrateOverYStaggeredTopFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverRFace(exp, level, 2, Some(1), offset)
  def integrateOverYStaggeredBottomFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverLFace(exp, level, 2, Some(1), offset)

  def integrateOverZStaggeredEastFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverRFace(exp, level, 0, Some(2), offset)
  def integrateOverZStaggeredWestFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverLFace(exp, level, 0, Some(2), offset)
  def integrateOverZStaggeredNorthFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverRFace(exp, level, 1, Some(2), offset)
  def integrateOverZStaggeredSouthFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverLFace(exp, level, 1, Some(2), offset)
  def integrateOverZStaggeredTopFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverRFace(exp, level, 2, Some(2), offset)
  def integrateOverZStaggeredBottomFace(exp : IR_Expression, level : Int, offset : Option[IR_ConstIndex]) : IR_Expression =
    integrateOverLFace(exp, level, 2, Some(2), offset)

  def integrateOverLFace(exp : IR_Expression, level : Int, faceDim : Int, stagDim : Option[Int], offset : Option[IR_ConstIndex]) : IR_Expression = {
    val newOffset = offset.getOrElse(IR_ConstIndex(Array.fill(Knowledge.dimensionality)(0)))
    newOffset(faceDim) -= 1

    integrateOverRFace(exp, level, faceDim, stagDim, Some(newOffset))
  }

  def doIntegrate(exp : IR_Expression, level : Int, faceDim : Int, stagDim : Option[Int], offset : Option[IR_ConstIndex]) = {
    def index = { // TODO: dim
      var i = IR_LoopOverDimensions.defIt(Knowledge.dimensionality)
      if (offset.isDefined) i += offset.get
      i
    }

    def offsetExp = {
      if (offset.isEmpty) {
        Duplicate(exp)
      } else {
        val wrapped = IR_ExpressionStatement(Duplicate(exp))
        IR_OffsetAllApplicable.offset = offset.get
        IR_OffsetAllApplicable.applyStandalone(wrapped)
        wrapped.expression
      }
    }

    Knowledge.dimensionality /* FIXME: numDims */ match {
      // TODO: refactor
      case 2 =>
        if (stagDim.isDefined) {
          val curStagDim = stagDim.get

          if (curStagDim == faceDim) {
            val compDim = if (0 == faceDim) 1 else 0
            geom.cellWidth(level, index, None, compDim) * offsetExp
          } else {
            geom.asInstanceOf[GridGeometry_staggered].stagCVWidth(level, index, None, curStagDim) * offsetExp
          }
        } else {
          val compDim = if (0 == faceDim) 1 else 0
          geom.cellWidth(level, index, None, compDim) * offsetExp
        }
      case 3 =>
        if (stagDim.isDefined) {
          val curStagDim = stagDim.get

          if (curStagDim == faceDim) {
            val compDim0 = if (0 == faceDim) 1 else 0
            val compDim1 = if (2 == faceDim) 1 else 2

            geom.cellWidth(level, index, None, compDim0) * geom.cellWidth(level, index, None, compDim1) * offsetExp
          } else {
            val compDim = if (0 != faceDim && 0 != curStagDim) 0 else if (1 != faceDim && 1 != curStagDim) 1 else 2

            if (compDim < curStagDim)
              geom.cellWidth(level, index, None, compDim) * geom.asInstanceOf[GridGeometry_staggered].stagCVWidth(level, index, None, curStagDim) * offsetExp
            else
              geom.asInstanceOf[GridGeometry_staggered].stagCVWidth(level, index, None, curStagDim) * geom.cellWidth(level, index, None, compDim) * offsetExp
          }
        } else {
          val compDim0 = if (0 == faceDim) 1 else 0
          val compDim1 = if (2 == faceDim) 1 else 2

          geom.cellWidth(level, index, None, compDim0) * geom.cellWidth(level, index, None, compDim1) * offsetExp
        }
    }
  }

  // integration over faces of staggered CVs is done by defining stagDim - the dimension in which the grid is staggered
  // expS instead of exp : IR_Expression to allow for match and replace of the original expression
  def integrateOverRFace(exp : IR_Expression, level : Int, faceDim : Int, stagDim : Option[Int], offset : Option[IR_ConstIndex]) : IR_Expression =
  integrateOverRFace(IR_ExpressionStatement(exp), level, faceDim, stagDim, offset)
  def integrateOverRFace(expS : IR_ExpressionStatement, level : Int, faceDim : Int, stagDim : Option[Int], offset : Option[IR_ConstIndex]) : IR_Expression = {
    def exp = expS.expression

    // check if there are any field accesses in the current (sub-)expression
    IR_CollectFieldAccess.applyStandalone(expS)

    if (0 == IR_CollectFieldAccess.fieldAccesses.length) {
      // no field accesses => check for virtual field accesses
      val levels = StateManager.findAll[IR_VirtualFieldAccess](expS).map(_.level)
      if (levels.exists(_ != level)) {
        Logger.warn(s"Mixed level integration is currently not supported (${ exp.prettyprint })")
        return exp
      }
      return doIntegrate(exp, level, faceDim, stagDim, offset)
    }

    // check if all occurring level specifications are identical
    for (fieldAccess <- IR_CollectFieldAccess.fieldAccesses) {
      if (level != fieldAccess.fieldSelection.level) {
        Logger.warn(s"Mixed level field integration is currently not supported (${ exp.prettyprint })")
        return exp
      }
    }
    for (fieldAccess <- IR_CollectFieldAccess.vFieldAccesses) { // check virtual fields as well
      if (level != fieldAccess.level) {
        Logger.warn(s"Mixed level field integration is currently not supported (${ exp.prettyprint })")
        return exp
      }
    }

    // step 1: wrap field accesses with eval functions if necessary
    object WrappingFieldAccesses extends QuietDefaultStrategy("Wrapping field accesses") {
      val pIntAnnot = "PIECEWISE_INTEGRATION"
      def addPIntAnnot(exp : IR_Expression) = { exp.annotate(pIntAnnot); exp }

      this += new Transformation("Wrapping", {
        case fieldAccess : IR_FieldAccess =>
          val discr = fieldAccess.fieldSelection.field.discretization
          if (stagDim.isDefined) {
            val curStagDim = stagDim.get
            discr match {
              case "cell" if curStagDim == faceDim                                                                        => fieldAccess // direct sampling
              case "cell" if curStagDim != faceDim                                                                        => addPIntAnnot(EvalAtRFace(GridUtil.offsetAccess(fieldAccess, -1, curStagDim), level, faceDim, stagDim)) // interpolation with offset, piecewiseIntegration
              case fieldDiscr @ ("face_x" | "face_y" | "face_z") if s"face_${ IR_DimToString(curStagDim) }" == fieldDiscr => // field discretization matches CV
                EvalAtRFace(fieldAccess, level, faceDim, stagDim) // interpolation
              case "face_x" if 0 != curStagDim                                                                            => // -1 in CV's stag dim and +1 in field discretization's stag dim
                addPIntAnnot(GridUtil.offsetAccess(GridUtil.offsetAccess(fieldAccess, -1, curStagDim), 1, 0)) // direct sampling with offset, piecewiseIntegration
              case "face_y" if 1 != curStagDim                                                                            => // -1 in CV's stag dim and +1 in field discretization's stag dim
                addPIntAnnot(GridUtil.offsetAccess(GridUtil.offsetAccess(fieldAccess, -1, curStagDim), 1, 1)) // direct sampling with offset, piecewiseIntegration
              case "face_z" if 2 != curStagDim                                                                            => // -1 in CV's stag dim and +1 in field discretization's stag dim
                addPIntAnnot(GridUtil.offsetAccess(GridUtil.offsetAccess(fieldAccess, -1, curStagDim), 1, 2)) // direct sampling with offset, piecewiseIntegration
              case _                                                                                                      => Logger.error(s"Unknown or unsupported discretization $discr for field $fieldAccess in staggered integration")
            }
          } else {
            discr match {
              case "cell"                         => EvalAtRFace(fieldAccess, level, faceDim, stagDim) // interpolation
              case "face_x" | "face_y" | "face_z" =>
                if (s"face_${ IR_DimToString(faceDim) }" == discr)
                  GridUtil.offsetAccess(fieldAccess, 1, faceDim) // direct sampling with offset
                else
                  addPIntAnnot(EvalAtRFace(fieldAccess, level, faceDim, stagDim)) // interpolation, piecewiseIntegration
              case _                              => Logger.error(s"Unknown or unsupported discretization $discr for field $fieldAccess in basic integration")
            }
          }

        case fieldAccess : IR_VirtualFieldAccess =>
          Logger.warn(s"Virtual field accesses ($fieldAccess) are currently unsupported within evaluation and integration functions")
          fieldAccess

        case eval : EvalAtRFace =>
          if (eval.faceDim != faceDim) Logger.error(s"Found unaligned eval for faceDim ${ eval.faceDim } in integration for faceDim $faceDim in eval for ${ eval.fieldAccess }")
          if (eval.stagDim != stagDim) Logger.error(s"Found unaligned eval for stagDim ${ eval.stagDim } in integration for stagDim $stagDim in eval for ${ eval.fieldAccess }")

          val discr = eval.fieldAccess.fieldSelection.field.discretization
          if (stagDim.isDefined) {
            val curStagDim = stagDim.get
            discr match {
              case "cell" if curStagDim == faceDim                                                                        => eval.fieldAccess // direct sampling
              case "cell" if curStagDim != faceDim                                                                        =>
                eval.fieldAccess = GridUtil.offsetAccess(eval.fieldAccess, -1, curStagDim)
                addPIntAnnot(eval) // interpolation with offset, piecewiseIntegration
              case fieldDiscr @ ("face_x" | "face_y" | "face_z") if s"face_${ IR_DimToString(curStagDim) }" == fieldDiscr => // field discretization matches CV
                eval // orig eval is fine
              case "face_x" if 0 != curStagDim                                                                            => // -1 in CV's stag dim and +1 in field discretization's stag dim => ignore eval as direct sampling is possible
                addPIntAnnot(GridUtil.offsetAccess(GridUtil.offsetAccess(eval.fieldAccess, -1, curStagDim), 1, 0)) // direct sampling with offset, piecewiseIntegration
              case "face_y" if 1 != curStagDim                                                                            =>
                addPIntAnnot(GridUtil.offsetAccess(GridUtil.offsetAccess(eval.fieldAccess, -1, curStagDim), 1, 1))
              case "face_z" if 2 != curStagDim                                                                            =>
                addPIntAnnot(GridUtil.offsetAccess(GridUtil.offsetAccess(eval.fieldAccess, -1, curStagDim), 1, 2))
              case _                                                                                                      => Logger.error(s"Unknown or unsupported discretization $discr for field ${ eval.fieldAccess } in staggered integration")
            }
          } else {
            discr match {
              case "cell"                         => eval // interpolation
              case "face_x" | "face_y" | "face_z" =>
                if (s"face_${ IR_DimToString(faceDim) }" == discr) {
                  eval.fieldAccess = GridUtil.offsetAccess(eval.fieldAccess, 1, faceDim)
                  eval // direct sampling with offset
                } else
                  addPIntAnnot(eval) // interpolation, piecewiseIntegration
              case _                              => Logger.error(s"Unknown or unsupported discretization $discr for field ${ eval.fieldAccess } in basic integration")
            }
          }

        case _ : IR_IntegrateOnGrid =>
          Logger.error("Integration functions called inside other integration functions are currently not supported")
      }, false) // not recursive -> don't look inside eval functions
    }
    WrappingFieldAccesses.applyStandalone(expS)

    // step 2: check if integration by parts is required
    val piecewiseIntegration = StateManager.findFirst({ n : Node => n.hasAnnotation(WrappingFieldAccesses.pIntAnnot) }, expS).isDefined

    // step 3: apply chosen integration
    object ShiftFieldAccessIndices extends QuietDefaultStrategy("Shifting indices of field accesses") {
      var offset : IR_Expression = 0
      var dim : Int = 0
      var requiredAnnot : Option[String] = None

      this += new Transformation("Searching and shifting", {
        case fieldAccess : IR_FieldAccess if requiredAnnot.isEmpty || fieldAccess.hasAnnotation(requiredAnnot.get)        =>
          fieldAccess.index(dim) += offset
          fieldAccess
        case fieldAccess : IR_VirtualFieldAccess if requiredAnnot.isEmpty || fieldAccess.hasAnnotation(requiredAnnot.get) =>
          fieldAccess.index(dim) += offset
          fieldAccess
        case eval : EvalAtRFace if requiredAnnot.isEmpty || eval.hasAnnotation(requiredAnnot.get)                         =>
          eval.fieldAccess.index(dim) += offset
          eval
      }, false) // skip field accesses inside eval nodes
    }

    if (piecewiseIntegration) {
      def index = { // TODO: dim
        var i = IR_LoopOverDimensions.defIt(Knowledge.dimensionality)
        if (offset.isDefined) i += offset.get
        i
      }

      if (stagDim.isDefined) {
        val curStagDim = stagDim.get
        if (curStagDim == faceDim)
          Logger.error("piecewise integration on faces in the stagger dimension of staggered cells is not supported")

        val centerExp = IR_ExpressionStatement(Duplicate(exp))
        val offsetExp = IR_ExpressionStatement(Duplicate(exp))

        if (offset.isDefined) {
          IR_OffsetAllApplicable.offset = offset.get
          IR_OffsetAllApplicable.applyStandalone(centerExp)
          IR_OffsetAllApplicable.applyStandalone(offsetExp)
        }

        ShiftFieldAccessIndices.offset = 1
        ShiftFieldAccessIndices.dim = curStagDim
        ShiftFieldAccessIndices.requiredAnnot = Some(WrappingFieldAccesses.pIntAnnot)
        ShiftFieldAccessIndices.applyStandalone(offsetExp)

        def cellCenterToFace(dim : Int, level : Int, index : IR_ExpressionIndex) = 0.5 * IR_VirtualFieldAccess(IR_VF_CellWidthPerDim.find(level, dim), index)

        Knowledge.dimensionality /* FIXME: numDims */ match {
          // TODO: refactor
          case 2 =>
            (cellCenterToFace(curStagDim, level, IR_GridUtil.offsetIndex(index, -1, curStagDim)) * centerExp.expression
              + cellCenterToFace(curStagDim, level, index) * offsetExp.expression)

          case 3 =>
            val compDim = if (0 != faceDim && 0 != curStagDim) 0 else if (1 != faceDim && 1 != curStagDim) 1 else 2

            IR_VirtualFieldAccess(IR_VF_CellWidthPerDim.find(level, compDim), index) *
              (cellCenterToFace(curStagDim, level, IR_GridUtil.offsetIndex(index, -1, curStagDim)) * centerExp.expression
                + cellCenterToFace(curStagDim, level, index) * offsetExp.expression)
        }
      } else {
        Logger.error("piecewise integration on non-staggered cell interfaces is not supported")
      }
    } else {
      doIntegrate(exp, level, faceDim, stagDim, offset)
    }
  }

}
