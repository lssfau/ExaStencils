package exastencils.grid

import exastencils.base.ir._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.prettyprinting._

abstract class GridEvaluator() {
  def invokeEvalResolve(functionName : String, fieldAccess : FieldAccess, interpolation : String) : IR_Expression = {
    val method = this.getClass().getMethods.find(_.getName == functionName)
    if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
    method.get.invoke(this, fieldAccess, interpolation).asInstanceOf[IR_Expression]
  }

  def invokeIntegrateResolve(functionName : String, exp : IR_Expression) : IR_Expression = {
    val method = this.getClass().getMethods.find(_.getName == functionName)
    if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
    method.get.invoke(this, exp).asInstanceOf[IR_Expression]
  }
}

object GridEvaluator {
  def getEvaluator = {
    if (Knowledge.grid_isAxisAligned) GridEvaluator_AxisAligned
    else Logger.error("Evaluators for non-axis-aligned grids are currently not supported")
  }
}

object GridEvaluator_AxisAligned extends GridEvaluator {
  def geom = GridGeometry.getGeometry.asInstanceOf[GridGeometry_staggered]

  // evaluations and interpolations
  def evalAtEastFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 0, None, interpolation)
  def evalAtWestFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 0, None, interpolation)
  def evalAtNorthFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 1, None, interpolation)
  def evalAtSouthFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 1, None, interpolation)
  def evalAtTopFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 2, None, interpolation)
  def evalAtBottomFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 2, None, interpolation)

  def evalAtXStaggeredEastFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 0, Some(0), interpolation)
  def evalAtXStaggeredWestFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 0, Some(0), interpolation)
  def evalAtXStaggeredNorthFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 1, Some(0), interpolation)
  def evalAtXStaggeredSouthFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 1, Some(0), interpolation)
  def evalAtXStaggeredTopFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 2, Some(0), interpolation)
  def evalAtXStaggeredBottomFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 2, Some(0), interpolation)

  def evalAtYStaggeredEastFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 0, Some(1), interpolation)
  def evalAtYStaggeredWestFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 0, Some(1), interpolation)
  def evalAtYStaggeredNorthFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 1, Some(1), interpolation)
  def evalAtYStaggeredSouthFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 1, Some(1), interpolation)
  def evalAtYStaggeredTopFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 2, Some(1), interpolation)
  def evalAtYStaggeredBottomFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 2, Some(1), interpolation)

  def evalAtZStaggeredEastFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 0, Some(2), interpolation)
  def evalAtZStaggeredWestFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 0, Some(2), interpolation)
  def evalAtZStaggeredNorthFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 1, Some(2), interpolation)
  def evalAtZStaggeredSouthFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 1, Some(2), interpolation)
  def evalAtZStaggeredTopFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 2, Some(2), interpolation)
  def evalAtZStaggeredBottomFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 2, Some(2), interpolation)

  def evalAtLFace(fieldAccess : FieldAccess, faceDim : Int, stagDim : Option[Int], interpolation : String = "default") =
    EvalAtRFace(GridUtil.offsetAccess(fieldAccess, -1, faceDim), faceDim, stagDim, interpolation)

  case class EvalAtRFace(var fieldAccess : FieldAccess, var faceDim : Int, var stagDim : Option[Int], var interpolation : String = "default") extends IR_Expression {
    override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = EvalAtRFace\n"

    def expandSpecial : Output[IR_Expression] = {
      val field = fieldAccess.fieldSelection.field
      val baseIndex = fieldAccess.index
      val level = field.level

      var a0 : (() => IR_Expression) = (() => { IR_NullExpression })
      var a1 : (() => IR_Expression) = (() => { IR_NullExpression })
      var x0 = (() => { Duplicate(fieldAccess) })
      var x1 = (() => { GridUtil.offsetAccess(fieldAccess, 1, faceDim) })

      field.discretization match {
        case "cell"                         => {
          if (stagDim.isDefined) {
            return fieldAccess // value is located at the evaluation region
          } else {
            a0 = (() => { geom.cellCenterToFace(level, Duplicate(baseIndex), None, faceDim) })
            a1 = (() => { geom.cellCenterToFace(level, GridUtil.offsetIndex(baseIndex, 1, faceDim), None, faceDim) })
          }
        }
        case "face_x" | "face_y" | "face_z" => {
          if (stagDim.isDefined) {
            if (s"face_${ dimToString(faceDim) }" == field.discretization) {
              // interpolation weights are always 0.5 due to geometric construction
              a0 = (() => { 0.5 })
              a1 = (() => { 0.5 })
            } else {
              Logger.warn(s"Trying to evaluate $fieldAccess on face dimension $faceDim of a ${ stagDim.get } staggered CV. This is not unique. Defaulting to leftmost candidate")
              return GridUtil.offsetAccess(fieldAccess, 1, stagDim.get)
            }
          } else {
            return fieldAccess // value is located at the evaluation region
          }
        }
      }

      // compile evaluation
      interpolation match {
        case "linear" | "default" => (a1() * x0() + a0() * x1()) / (a0() + a1())
        case "harmonicMean"       => ((a0() + a1()) * (x0() * x1())) / (a1() * x0() + a0() * x1())
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
    ShiftFieldAccessIndices.offset = -1
    ShiftFieldAccessIndices.dim = faceDim
    ShiftFieldAccessIndices.applyStandalone(IR_ExpressionStatement(exp))

    integrateOverRFace(exp, faceDim, stagDim)
  }

  // integration over faces of staggered CVs is done by defining stagDim - the dimension in which the grid is staggered
  def integrateOverRFace(exp : IR_Expression, faceDim : Int, stagDim : Option[Int]) : IR_Expression = {
    // check if there are any field accesses in the current (sub-)expression
    CollectFieldAccesses.applyStandalone(IR_Scope(exp))

    // TODO: find a way to handle constants
    if (0 == CollectFieldAccesses.fieldAccesses.size) {
      Logger.warn(s"Trying to evaluate index-independent expression ${ exp.prettyprint } - currently unsupported")
      return exp
    }

    // check if all occurring level specifications are identical
    val level = CollectFieldAccesses.fieldAccesses(0).fieldSelection.level
    for (fieldAccess <- CollectFieldAccesses.fieldAccesses) {
      if (level != fieldAccess.fieldSelection.level) {
        Logger.warn(s"Mixed level field integration is currently not supported (${ exp.prettyprint })")
        return exp
      }
    }
    for (fieldAccess <- CollectFieldAccesses.vFieldAccesses) { // check virtual fields as well
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
        case fieldAccess : FieldAccess                                                                                            => {
          val discr = fieldAccess.fieldSelection.field.discretization
          if (stagDim.isDefined) {
            val curStagDim = stagDim.get
            discr match {
              case "cell" if curStagDim == faceDim                                                                       => fieldAccess // direct sampling
              case "cell" if curStagDim != faceDim                                                                       => addPIntAnnot(EvalAtRFace(GridUtil.offsetAccess(fieldAccess, -1, curStagDim), faceDim, stagDim)) // interpolation with offset, piecewiseIntegration
              case fieldDiscr @ ("face_x" | "face_y" | "face_z") if (s"face_${ dimToString(curStagDim) }" == fieldDiscr) => // field discretization matches CV
                EvalAtRFace(fieldAccess, faceDim, stagDim) // interpolation
              case "face_x" if 0 != curStagDim                                                                           => // -1 in CV's stag dim and +1 in field discretization's stag dim
                addPIntAnnot(GridUtil.offsetAccess(GridUtil.offsetAccess(fieldAccess, -1, curStagDim), 1, 0)) // direct sampling with offset, piecewiseIntegration
              case "face_y" if 1 != curStagDim                                                                           => // -1 in CV's stag dim and +1 in field discretization's stag dim
                addPIntAnnot(GridUtil.offsetAccess(GridUtil.offsetAccess(fieldAccess, -1, curStagDim), 1, 1)) // direct sampling with offset, piecewiseIntegration
              case "face_z" if 2 != curStagDim                                                                           => // -1 in CV's stag dim and +1 in field discretization's stag dim
                addPIntAnnot(GridUtil.offsetAccess(GridUtil.offsetAccess(fieldAccess, -1, curStagDim), 1, 2)) // direct sampling with offset, piecewiseIntegration
              case _                                                                                                     => Logger.error(s"Unknown or unsupported discretization $discr for field $fieldAccess in staggered integration")
            }
          } else {
            discr match {
              case "cell"                         => EvalAtRFace(fieldAccess, faceDim, stagDim) // interpolation
              case "face_x" | "face_y" | "face_z" => {
                if (s"face_${ dimToString(faceDim) }" == discr)
                  GridUtil.offsetAccess(fieldAccess, 1, faceDim) // direct sampling with offset
                else
                  addPIntAnnot(EvalAtRFace(fieldAccess, faceDim, stagDim)) // interpolation, piecewiseIntegration
              }
              case _                              => Logger.error(s"Unknown or unsupported discretization $discr for field $fieldAccess in basic integration")
            }
          }
        }
        case fieldAccess : VirtualFieldAccess                                                                                     => {
          Logger.warn(s"Virtual field accesses ($fieldAccess) are currently unsupported within evaluation and intergration functions")
          fieldAccess
        }
        case eval : EvalAtRFace                                                                                                   => {
          if (eval.faceDim != faceDim) Logger.error(s"Found unaligned eval for faceDim ${ eval.faceDim } in integration for faceDim $faceDim in eval for ${ eval.fieldAccess }")
          if (eval.stagDim != stagDim) Logger.error(s"Found unaligned eval for stagDim ${ eval.stagDim } in integration for stagDim $stagDim in eval for ${ eval.fieldAccess }")

          val discr = eval.fieldAccess.fieldSelection.field.discretization
          if (stagDim.isDefined) {
            val curStagDim = stagDim.get
            discr match {
              case "cell" if curStagDim == faceDim                                                                       => eval.fieldAccess // direct sampling
              case "cell" if curStagDim != faceDim                                                                       =>
                eval.fieldAccess = GridUtil.offsetAccess(eval.fieldAccess, -1, curStagDim)
                addPIntAnnot(eval) // interpolation with offset, piecewiseIntegration
              case fieldDiscr @ ("face_x" | "face_y" | "face_z") if (s"face_${ dimToString(curStagDim) }" == fieldDiscr) => // field discretization matches CV
                eval // orig eval is fine
              case "face_x" if 0 != curStagDim                                                                           => // -1 in CV's stag dim and +1 in field discretization's stag dim => ignore eval as direct sampling is possible
                addPIntAnnot(GridUtil.offsetAccess(GridUtil.offsetAccess(eval.fieldAccess, -1, curStagDim), 1, 0)) // direct sampling with offset, piecewiseIntegration
              case "face_y" if 1 != curStagDim                                                                           =>
                addPIntAnnot(GridUtil.offsetAccess(GridUtil.offsetAccess(eval.fieldAccess, -1, curStagDim), 1, 1))
              case "face_z" if 2 != curStagDim                                                                           =>
                addPIntAnnot(GridUtil.offsetAccess(GridUtil.offsetAccess(eval.fieldAccess, -1, curStagDim), 1, 2))
              case _                                                                                                     => Logger.error(s"Unknown or unsupported discretization $discr for field ${ eval.fieldAccess } in staggered integration")
            }
          } else {
            discr match {
              case "cell"                         => eval // interpolation
              case "face_x" | "face_y" | "face_z" => {
                if (s"face_${ dimToString(faceDim) }" == discr) {
                  eval.fieldAccess = GridUtil.offsetAccess(eval.fieldAccess, 1, faceDim)
                  eval // direct sampling with offset
                } else
                  addPIntAnnot(eval) // interpolation, piecewiseIntegration
              }
              case _                              => Logger.error(s"Unknown or unsupported discretization $discr for field ${ eval.fieldAccess } in basic integration")
            }
          }
        }
        case fctCall @ FunctionCallExpression(functionName, args) if ResolveIntegrationFunctions.functions.contains(functionName) => {
          Logger.error("Integration functions called inside other integration functions are currently not supported")
        }
      }, false) // not recursive -> don't look inside eval functions
    }
    WrappingFieldAccesses.applyStandalone(IR_Scope(exp))

    // step 2: check if integration by parts is required
    var piecewiseIntegration = StateManager.findFirst({ n : Node => n.hasAnnotation(WrappingFieldAccesses.pIntAnnot) }, IR_Scope(exp)).isDefined

    // step 3: apply chosen integration
    object ShiftFieldAccessIndices_ extends QuietDefaultStrategy("Shifting indices of field accesses") {
      var offset : IR_Expression = 0
      var dim : Int = 0
      var requiredAnnot : Option[String] = None

      this += new Transformation("Searching and shifting", {
        case fieldAccess : FieldAccess if requiredAnnot.isEmpty || fieldAccess.hasAnnotation(requiredAnnot.get)        =>
          fieldAccess.index(dim) += offset
          fieldAccess
        case fieldAccess : VirtualFieldAccess if requiredAnnot.isEmpty || fieldAccess.hasAnnotation(requiredAnnot.get) =>
          fieldAccess.index(dim) += offset
          fieldAccess
        case eval : EvalAtRFace if requiredAnnot.isEmpty || eval.hasAnnotation(requiredAnnot.get)                      =>
          eval.fieldAccess.index(dim) += offset
          eval
      }, false) // skip field accesses inside eval nodes
    }

    if (piecewiseIntegration) {
      def index = LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim

      if (stagDim.isDefined) {
        val curStagDim = stagDim.get
        if (curStagDim == faceDim)
          Logger.error("piecewise integration on faces in the stagger dimension of staggered cells is not supported")

        val compDim = (if (0 != faceDim && 0 != curStagDim) 0 else (if (1 != faceDim && 1 != curStagDim) 1 else 2))

        val centerExp = Duplicate(exp)
        val offsetExp = Duplicate(exp)

        ShiftFieldAccessIndices_.offset = 1
        ShiftFieldAccessIndices_.dim = curStagDim
        ShiftFieldAccessIndices_.requiredAnnot = Some(WrappingFieldAccesses.pIntAnnot)
        ShiftFieldAccessIndices_.applyStandalone(IR_ExpressionStatement(offsetExp))

        (VirtualFieldAccess(s"vf_cellWidth_${ dimToString(compDim) }", level, index) *
          (VirtualFieldAccess(s"vf_cellCenterToFace_${ dimToString(curStagDim) }", level, GridUtil.offsetIndex(index, -1, curStagDim)) * centerExp
            + VirtualFieldAccess(s"vf_cellCenterToFace_${ dimToString(curStagDim) }", level, index) * offsetExp))
      } else {
        Logger.error("piecewise integration on non-staggered cell interfaces is not supported")
      }
    } else {
      def index = LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim

      if (stagDim.isDefined) {
        val curStagDim = stagDim.get

        if (curStagDim == faceDim) {
          val compDim0 = (if (0 == faceDim) 1 else 0)
          val compDim1 = (if (2 == faceDim) 1 else 2)

          geom.cellWidth(level, index, None, compDim0) * geom.cellWidth(level, index, None, compDim1) * exp
        } else {
          val compDim = (if (0 != faceDim && 0 != curStagDim) 0 else (if (1 != faceDim && 1 != curStagDim) 1 else 2))

          geom.cellWidth(level, index, None, compDim) * geom.stagCVWidth(level, index, None, curStagDim) * exp
        }
      } else {
        val compDim0 = (if (0 == faceDim) 1 else 0)
        val compDim1 = (if (2 == faceDim) 1 else 2)

        geom.cellWidth(level, index, None, compDim0) * geom.cellWidth(level, index, None, compDim1) * exp
      }
    }
  }
}
