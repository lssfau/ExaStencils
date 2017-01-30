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
  def evalAtEastFace(fieldAccess : IR_FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 0, None, interpolation)
  def evalAtWestFace(fieldAccess : IR_FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 0, None, interpolation)
  def evalAtNorthFace(fieldAccess : IR_FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 1, None, interpolation)
  def evalAtSouthFace(fieldAccess : IR_FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 1, None, interpolation)
  def evalAtTopFace(fieldAccess : IR_FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 2, None, interpolation)
  def evalAtBottomFace(fieldAccess : IR_FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 2, None, interpolation)

  def evalAtXStaggeredEastFace(fieldAccess : IR_FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 0, Some(0), interpolation)
  def evalAtXStaggeredWestFace(fieldAccess : IR_FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 0, Some(0), interpolation)
  def evalAtXStaggeredNorthFace(fieldAccess : IR_FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 1, Some(0), interpolation)
  def evalAtXStaggeredSouthFace(fieldAccess : IR_FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 1, Some(0), interpolation)
  def evalAtXStaggeredTopFace(fieldAccess : IR_FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 2, Some(0), interpolation)
  def evalAtXStaggeredBottomFace(fieldAccess : IR_FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 2, Some(0), interpolation)

  def evalAtYStaggeredEastFace(fieldAccess : IR_FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 0, Some(1), interpolation)
  def evalAtYStaggeredWestFace(fieldAccess : IR_FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 0, Some(1), interpolation)
  def evalAtYStaggeredNorthFace(fieldAccess : IR_FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 1, Some(1), interpolation)
  def evalAtYStaggeredSouthFace(fieldAccess : IR_FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 1, Some(1), interpolation)
  def evalAtYStaggeredTopFace(fieldAccess : IR_FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 2, Some(1), interpolation)
  def evalAtYStaggeredBottomFace(fieldAccess : IR_FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 2, Some(1), interpolation)

  def evalAtZStaggeredEastFace(fieldAccess : IR_FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 0, Some(2), interpolation)
  def evalAtZStaggeredWestFace(fieldAccess : IR_FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 0, Some(2), interpolation)
  def evalAtZStaggeredNorthFace(fieldAccess : IR_FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 1, Some(2), interpolation)
  def evalAtZStaggeredSouthFace(fieldAccess : IR_FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 1, Some(2), interpolation)
  def evalAtZStaggeredTopFace(fieldAccess : IR_FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 2, Some(2), interpolation)
  def evalAtZStaggeredBottomFace(fieldAccess : IR_FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 2, Some(2), interpolation)

  def evalAtLFace(fieldAccess : IR_FieldAccess, faceDim : Int, stagDim : Option[Int], interpolation : String = "default") =
    EvalAtRFace(GridUtil.offsetAccess(fieldAccess, -1, faceDim), faceDim, stagDim, interpolation)

  case class EvalAtRFace(var fieldAccess : IR_FieldAccess, var faceDim : Int, var stagDim : Option[Int], var interpolation : String = "default") extends IR_Expression with IR_SpecialExpandable {
    override def datatype = IR_UnitDatatype

    def expandSpecial : Output[IR_Expression] = {
      val field = fieldAccess.fieldSelection.field
      val baseIndex = fieldAccess.index
      val level = field.level

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
              Logger.warn(s"Trying to evaluate $fieldAccess on face dimension $faceDim of a ${ stagDim.get } staggered CV. This is not unique. Defaulting to leftmost candidate")
              return GridUtil.offsetAccess(fieldAccess, 1, stagDim.get)
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
          EvalAtRFace(fieldAccess, faceDim, stagDim, "default").expandSpecial
      }
    }
  }

  // integrations
  def integrateOverEastFace(exp : IR_Expression) : IR_Expression = integrateOverRFace(exp, 0, None)
  def integrateOverWestFace(exp : IR_Expression) : IR_Expression = integrateOverLFace(exp, 0, None)
  def integrateOverNorthFace(exp : IR_Expression) : IR_Expression = integrateOverRFace(exp, 1, None)
  def integrateOverSouthFace(exp : IR_Expression) : IR_Expression = integrateOverLFace(exp, 1, None)
  def integrateOverTopFace(exp : IR_Expression) : IR_Expression = integrateOverRFace(exp, 2, None)
  def integrateOverBottomFace(exp : IR_Expression) : IR_Expression = integrateOverLFace(exp, 2, None)

  def integrateOverXStaggeredEastFace(exp : IR_Expression) : IR_Expression = integrateOverRFace(exp, 0, Some(0))
  def integrateOverXStaggeredWestFace(exp : IR_Expression) : IR_Expression = integrateOverLFace(exp, 0, Some(0))
  def integrateOverXStaggeredNorthFace(exp : IR_Expression) : IR_Expression = integrateOverRFace(exp, 1, Some(0))
  def integrateOverXStaggeredSouthFace(exp : IR_Expression) : IR_Expression = integrateOverLFace(exp, 1, Some(0))
  def integrateOverXStaggeredTopFace(exp : IR_Expression) : IR_Expression = integrateOverRFace(exp, 2, Some(0))
  def integrateOverXStaggeredBottomFace(exp : IR_Expression) : IR_Expression = integrateOverLFace(exp, 2, Some(0))

  def integrateOverYStaggeredEastFace(exp : IR_Expression) : IR_Expression = integrateOverRFace(exp, 0, Some(1))
  def integrateOverYStaggeredWestFace(exp : IR_Expression) : IR_Expression = integrateOverLFace(exp, 0, Some(1))
  def integrateOverYStaggeredNorthFace(exp : IR_Expression) : IR_Expression = integrateOverRFace(exp, 1, Some(1))
  def integrateOverYStaggeredSouthFace(exp : IR_Expression) : IR_Expression = integrateOverLFace(exp, 1, Some(1))
  def integrateOverYStaggeredTopFace(exp : IR_Expression) : IR_Expression = integrateOverRFace(exp, 2, Some(1))
  def integrateOverYStaggeredBottomFace(exp : IR_Expression) : IR_Expression = integrateOverLFace(exp, 2, Some(1))

  def integrateOverZStaggeredEastFace(exp : IR_Expression) : IR_Expression = integrateOverRFace(exp, 0, Some(2))
  def integrateOverZStaggeredWestFace(exp : IR_Expression) : IR_Expression = integrateOverLFace(exp, 0, Some(2))
  def integrateOverZStaggeredNorthFace(exp : IR_Expression) : IR_Expression = integrateOverRFace(exp, 1, Some(2))
  def integrateOverZStaggeredSouthFace(exp : IR_Expression) : IR_Expression = integrateOverLFace(exp, 1, Some(2))
  def integrateOverZStaggeredTopFace(exp : IR_Expression) : IR_Expression = integrateOverRFace(exp, 2, Some(2))
  def integrateOverZStaggeredBottomFace(exp : IR_Expression) : IR_Expression = integrateOverLFace(exp, 2, Some(2))

  def integrateOverLFace(exp : IR_Expression, faceDim : Int, stagDim : Option[Int]) : IR_Expression = {
    IR_ShiftFieldAccessIndices.offset = -1
    IR_ShiftFieldAccessIndices.dim = faceDim
    IR_ShiftFieldAccessIndices.applyStandalone(IR_ExpressionStatement(exp))

    integrateOverRFace(exp, faceDim, stagDim)
  }

  // integration over faces of staggered CVs is done by defining stagDim - the dimension in which the grid is staggered
  // expS instead of exp : IR_Expression to allow for match and replace of the original expression
  def integrateOverRFace(exp : IR_Expression, faceDim : Int, stagDim : Option[Int]) : IR_Expression =
  integrateOverRFace(IR_ExpressionStatement(exp), faceDim, stagDim)
  def integrateOverRFace(expS : IR_ExpressionStatement, faceDim : Int, stagDim : Option[Int]) : IR_Expression = {
    def exp = expS.expression

    // check if there are any field accesses in the current (sub-)expression
    IR_CollectFieldAccess.applyStandalone(expS)

    // TODO: find a way to handle constants
    if (0 == IR_CollectFieldAccess.fieldAccesses.size) {
      Logger.warn(s"Trying to evaluate index-independent expression ${ exp.prettyprint } - currently unsupported")
      return exp
    }

    // check if all occurring level specifications are identical
    val level = IR_CollectFieldAccess.fieldAccesses(0).fieldSelection.level
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
              case "cell" if curStagDim != faceDim                                                                        => addPIntAnnot(EvalAtRFace(GridUtil.offsetAccess(fieldAccess, -1, curStagDim), faceDim, stagDim)) // interpolation with offset, piecewiseIntegration
              case fieldDiscr @ ("face_x" | "face_y" | "face_z") if s"face_${ IR_DimToString(curStagDim) }" == fieldDiscr => // field discretization matches CV
                EvalAtRFace(fieldAccess, faceDim, stagDim) // interpolation
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
              case "cell"                         => EvalAtRFace(fieldAccess, faceDim, stagDim) // interpolation
              case "face_x" | "face_y" | "face_z" =>
                if (s"face_${ IR_DimToString(faceDim) }" == discr)
                  GridUtil.offsetAccess(fieldAccess, 1, faceDim) // direct sampling with offset
                else
                  addPIntAnnot(EvalAtRFace(fieldAccess, faceDim, stagDim)) // interpolation, piecewiseIntegration
              case _                              => Logger.error(s"Unknown or unsupported discretization $discr for field $fieldAccess in basic integration")
            }
          }

        case fieldAccess : IR_VirtualFieldAccess =>
          Logger.warn(s"Virtual field accesses ($fieldAccess) are currently unsupported within evaluation and intergration functions")
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

        case fctCall @ IR_FunctionCall(function, args) if IR_ResolveIntegrateFunction.functions.contains(function.name) =>
          Logger.error("Integration functions called inside other integration functions are currently not supported")
      }, false) // not recursive -> don't look inside eval functions
    }
    WrappingFieldAccesses.applyStandalone(expS)

    // step 2: check if integration by parts is required
    val piecewiseIntegration = StateManager.findFirst({ n : Node => n.hasAnnotation(WrappingFieldAccesses.pIntAnnot) }, expS).isDefined

    // step 3: apply chosen integration
    object ShiftFieldAccessIndices_ extends QuietDefaultStrategy("Shifting indices of field accesses") {
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
      def index = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim

      if (stagDim.isDefined) {
        val curStagDim = stagDim.get
        if (curStagDim == faceDim)
          Logger.error("piecewise integration on faces in the stagger dimension of staggered cells is not supported")

        val compDim = if (0 != faceDim && 0 != curStagDim) 0 else if (1 != faceDim && 1 != curStagDim) 1 else 2

        val centerExp = Duplicate(exp)
        val offsetExp = Duplicate(exp)

        ShiftFieldAccessIndices_.offset = 1
        ShiftFieldAccessIndices_.dim = curStagDim
        ShiftFieldAccessIndices_.requiredAnnot = Some(WrappingFieldAccesses.pIntAnnot)
        ShiftFieldAccessIndices_.applyStandalone(IR_ExpressionStatement(offsetExp))

        IR_VirtualFieldAccess(s"vf_cellWidth_${ IR_DimToString(compDim) }", level, index) *
          (IR_VirtualFieldAccess(s"vf_cellCenterToFace_${ IR_DimToString(curStagDim) }", level, GridUtil.offsetIndex(index, -1, curStagDim)) * centerExp
            + IR_VirtualFieldAccess(s"vf_cellCenterToFace_${ IR_DimToString(curStagDim) }", level, index) * offsetExp)
      } else {
        Logger.error("piecewise integration on non-staggered cell interfaces is not supported")
      }
    } else {
      def index = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim

      if (stagDim.isDefined) {
        val curStagDim = stagDim.get

        if (curStagDim == faceDim) {
          val compDim0 = if (0 == faceDim) 1 else 0
          val compDim1 = if (2 == faceDim) 1 else 2

          geom.cellWidth(level, index, None, compDim0) * geom.cellWidth(level, index, None, compDim1) * exp
        } else {
          val compDim = if (0 != faceDim && 0 != curStagDim) 0 else if (1 != faceDim && 1 != curStagDim) 1 else 2

          geom.cellWidth(level, index, None, compDim) * geom.asInstanceOf[GridGeometry_staggered].stagCVWidth(level, index, None, curStagDim) * exp
        }
      } else {
        val compDim0 = if (0 == faceDim) 1 else 0
        val compDim1 = if (2 == faceDim) 1 else 2

        geom.cellWidth(level, index, None, compDim0) * geom.cellWidth(level, index, None, compDim1) * exp
      }
    }
  }

  object IR_ShiftFieldAccessIndices extends QuietDefaultStrategy("Shift indices of field accesses") {
    var offset : IR_Expression = 0
    var dim : Int = 0

    this += new Transformation("Searching and shifting", {
      case fieldAccess : IR_FieldAccess        =>
        fieldAccess.index(dim) += offset
        fieldAccess
      case fieldAccess : IR_VirtualFieldAccess =>
        fieldAccess.index(dim) += offset
        fieldAccess
    })
  }

}
