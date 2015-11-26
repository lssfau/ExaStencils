package exastencils.grid

import scala.annotation.migration
import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.prettyprinting.PpStream

object Grid_AxisAlignedVariableWidth extends Grid {
  override def initL4() : Unit = {
    val root = StateManager.root_.asInstanceOf[l4.Root]
    root.fieldLayouts += l4.LayoutDeclarationStatement(
      l4.LeveledIdentifier("DefNodeLineLayout", l4.FinestLevelSpecification()),
      l4.RealDatatype(), "Edge_Node".toLowerCase(),
      Some(l4.Index3D(1, 0, 0)), None,
      Some(l4.Index3D(1, 0, 0)), None,
      None)

    root.fields += l4.FieldDeclarationStatement(
      l4.LeveledIdentifier("node_pos_x", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout", None, 1, 0)
    if (Knowledge.dimensionality > 1)
      root.fields += l4.FieldDeclarationStatement(
        l4.LeveledIdentifier("node_pos_y", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout", None, 1, 0)
    if (Knowledge.dimensionality > 2)
      root.fields += l4.FieldDeclarationStatement(
        l4.LeveledIdentifier("node_pos_z", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout", None, 1, 0)

    root.fields += l4.FieldDeclarationStatement(
      l4.LeveledIdentifier("stag_cv_width_x", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout", None, 1, 0)
    if (Knowledge.dimensionality > 1)
      root.fields += l4.FieldDeclarationStatement(
        l4.LeveledIdentifier("stag_cv_width_y", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout", None, 1, 0)
    if (Knowledge.dimensionality > 2)
      root.fields += l4.FieldDeclarationStatement(
        l4.LeveledIdentifier("stag_cv_width_z", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout", None, 1, 0)
  }

  override def generateInitCode() = {
    /// node_pos      -> nodes of the original grid
    /// o   o   o   o   o
    /// cell_width      -> width of the control volumes of the original grid
    /// |---|   |---|
    /// stag_cv_width   -> width of the staggered control volumes
    /// |-|   |---|   |-|

    (0 until Knowledge.dimensionality).to[ListBuffer].map(dim => setupNodePos(dim, Knowledge.maxLevel)) ++
      (0 until Knowledge.dimensionality).to[ListBuffer].map(dim => setupStagCVWidth(dim, Knowledge.maxLevel))
  }

  def setupNodePos(dim : Integer, level : Integer) : Statement = {
    val expo = 1.5
    val numCells = (1 << level) // TODO: adapt for non-unit fragments
    val zoneSize = numCells / 4
    val step = 1.0 / zoneSize

    val field = FieldCollection.getFieldByIdentifier(s"node_pos_${dimToString(dim)}", level).get
    var baseIndex = LoopOverDimensions.defIt
    baseIndex(Knowledge.dimensionality) = 0
    val baseAccess = FieldAccess(FieldSelection(field, field.level, 0), baseIndex)

    val innerIt = LoopOverDimensions.defIt(0)

    LoopOverPoints(field, None, true,
      MultiIndex(-2, -1, -1), MultiIndex(-2, -1, -1), MultiIndex(1, 1, 1),
      ListBuffer[Statement](
        new ConditionStatement(LowerEqualExpression(innerIt, 0),
          AssignmentStatement(Duplicate(baseAccess), 0.0),
          new ConditionStatement(LowerEqualExpression(innerIt, 1 * zoneSize),
            AssignmentStatement(Duplicate(baseAccess), offsetAccess(baseAccess, -1 * innerIt + 0 * zoneSize, 0)
              + 0.0095 * FunctionCallExpression("pow", ListBuffer[Expression](step * (LoopOverDimensions.defIt(0) - 0.0 * zoneSize), expo))),
            new ConditionStatement(LowerEqualExpression(innerIt, 2 * zoneSize),
              AssignmentStatement(Duplicate(baseAccess), offsetAccess(baseAccess, -1 * innerIt + 1 * zoneSize, 0)
                + 0.0095 * step * (LoopOverDimensions.defIt(0) - 1.0 * zoneSize)),
              new ConditionStatement(LowerEqualExpression(innerIt, 3 * zoneSize),
                AssignmentStatement(Duplicate(baseAccess), offsetAccess(baseAccess, -1 * innerIt + 2 * zoneSize, 0)
                  + 0.0095 * step * (LoopOverDimensions.defIt(0) - 2.0 * zoneSize)),
                new ConditionStatement(LowerEqualExpression(innerIt, 4 * zoneSize),
                  AssignmentStatement(Duplicate(baseAccess), offsetAccess(baseAccess, -1 * innerIt + 3 * zoneSize, 0)
                    + 0.0095 * (1.0 - FunctionCallExpression("pow", ListBuffer[Expression](1.0 - step * (LoopOverDimensions.defIt(0) - 3.0 * zoneSize), expo)))),
                  AssignmentStatement(Duplicate(baseAccess), offsetAccess(baseAccess, -1, 0)))))))))
  }

  def setupStagCVWidth(dim : Integer, level : Integer) : Statement = {
    val expo = 1.5
    val numCells = (1 << level) // TODO: adapt for non-unit fragments
    val zoneSize = numCells / 4
    val step = 1.0 / zoneSize

    var baseIndex = LoopOverDimensions.defIt
    baseIndex(Knowledge.dimensionality) = 0
    val field = FieldCollection.getFieldByIdentifier(s"stag_cv_width_${dimToString(dim)}", level).get
    val baseAccess = FieldAccess(FieldSelection(field, field.level, 0), Duplicate(baseIndex))
    val npField = FieldCollection.getFieldByIdentifier(s"node_pos_${dimToString(dim)}", level).get
    val npBaseAccess = FieldAccess(FieldSelection(npField, npField.level, 0), Duplicate(baseIndex))

    val innerIt = LoopOverDimensions.defIt(0)

    LoopOverPoints(field, None, true,
      MultiIndex(-1, -1, -1), MultiIndex(-1, -1, -1), MultiIndex(1, 1, 1),
      ListBuffer[Statement](
        new ConditionStatement(EqEqExpression(0, innerIt),
          AssignmentStatement(Duplicate(baseAccess),
            0.5 * (Duplicate(npBaseAccess) + offsetAccess(npBaseAccess, 1, 0))
              - Duplicate(npBaseAccess)),
          new ConditionStatement(EqEqExpression(numCells, innerIt),
            AssignmentStatement(Duplicate(baseAccess),
              Duplicate(npBaseAccess)
                - 0.5 * (offsetAccess(npBaseAccess, -1, 0) + Duplicate(npBaseAccess))),
            AssignmentStatement(Duplicate(baseAccess),
              0.5 * (Duplicate(npBaseAccess) + offsetAccess(npBaseAccess, 1, 0))
                - 0.5 * (offsetAccess(npBaseAccess, -1, 0) + Duplicate(npBaseAccess)))))))
  }

  override def resolveGridMemberFunction(name : String) : Option[java.lang.reflect.Method] = {
    this.getClass().getMethods.find(_.getName.toLowerCase() == name.toLowerCase())
  }

  override def invokeEvalResolve(functionName : String, fieldAccess : FieldAccess, interpolation : String) : Expression = {
    val method = this.getClass().getMethods.find(_.getName == functionName)
    if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
    method.get.invoke(this, fieldAccess, interpolation).asInstanceOf[Expression]
  }

  override def invokeIntegrateResolve(functionName : String, exp : Expression) : Expression = {
    val method = this.getClass().getMethods.find(_.getName == functionName)
    if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
    method.get.invoke(this, exp).asInstanceOf[Expression]
  }

  def projectIdx(baseIndex : MultiIndex, dim : Int) = {
    new MultiIndex(baseIndex(dim), 0, 0, 0)
  }

  def offsetIndex(index : MultiIndex, offset : Expression, dim : Int) : MultiIndex = {
    var modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }
  def offsetAccess(fieldAccess : FieldAccess, offset : Expression, dim : Int) : FieldAccess = {
    var modAccess = Duplicate(fieldAccess)
    modAccess.index(dim) += offset
    modAccess
  }

  // direct accesses
  def nodePosition(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = FieldCollection.getFieldByIdentifierLevExp(s"node_pos_${dimToString(dim)}", level).get
    FieldAccess(FieldSelection(field, field.level, 0, arrayIndex), projectIdx(index, dim))
  }

  def stagCVWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = FieldCollection.getFieldByIdentifierLevExp(s"stag_cv_width_${dimToString(dim)}", level).get
    FieldAccess(FieldSelection(field, field.level, 0, arrayIndex), projectIdx(index, dim))
  }

  // compound accesses
  def cellWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    nodePosition(level, offsetIndex(index, 1, dim), arrayIndex, dim) - nodePosition(level, Duplicate(index), arrayIndex, dim)
  }

  def cellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int]) = {
    var exp : Expression = cellWidth(level, index, arrayIndex, 0)
    for (dim <- 1 until Knowledge.dimensionality)
      exp *= cellWidth(level, index, arrayIndex, dim)
    exp
  }

  def staggeredCellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int], stagDim : Int) = {
    var exp : Expression = (
      if (0 == stagDim)
        stagCVWidth(level, index, arrayIndex, 0)
      else
        cellWidth(level, index, arrayIndex, 0))
    for (dim <- 1 until Knowledge.dimensionality)
      if (dim == stagDim)
        exp *= stagCVWidth(level, index, arrayIndex, dim)
      else
        exp *= cellWidth(level, index, arrayIndex, dim)
    exp
  }

  def xStagCellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int]) = staggeredCellVolume(level, index, arrayIndex, 0)
  def yStagCellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int]) = staggeredCellVolume(level, index, arrayIndex, 1)
  def zStagCellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int]) = staggeredCellVolume(level, index, arrayIndex, 2)

  def cellCenterToFace(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    0.5 * cellWidth(level, index, arrayIndex, dim)
  }

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
  def evalAtXStaggeredSouthFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 0, Some(1), interpolation)
  def evalAtXStaggeredTopFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 2, Some(0), interpolation)
  def evalAtXStaggeredBottomFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 0, Some(2), interpolation)

  def evalAtYStaggeredEastFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 0, Some(1), interpolation)
  def evalAtYStaggeredWestFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 1, Some(0), interpolation)
  def evalAtYStaggeredNorthFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 1, Some(1), interpolation)
  def evalAtYStaggeredSouthFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 1, Some(1), interpolation)
  def evalAtYStaggeredTopFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 2, Some(1), interpolation)
  def evalAtYStaggeredBottomFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 1, Some(2), interpolation)

  def evalAtZStaggeredEastFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 0, Some(2), interpolation)
  def evalAtZStaggeredWestFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 2, Some(0), interpolation)
  def evalAtZStaggeredNorthFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 1, Some(2), interpolation)
  def evalAtZStaggeredSouthFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 2, Some(1), interpolation)
  def evalAtZStaggeredTopFace(fieldAccess : FieldAccess, interpolation : String) = EvalAtRFace(fieldAccess, 2, Some(2), interpolation)
  def evalAtZStaggeredBottomFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 2, Some(2), interpolation)

  def evalAtLFace(fieldAccess : FieldAccess, faceDim : Int, stagDim : Option[Int], interpolation : String = "default") =
    EvalAtRFace(offsetAccess(fieldAccess, -1, faceDim), faceDim, stagDim, interpolation)

  case class EvalAtRFace(var fieldAccess : FieldAccess, var faceDim : Int, var stagDim : Option[Int], var interpolation : String = "default") extends Expression {
    override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = EvalAtRFace\n"

    def expandSpecial : Output[Expression] = {
      val field = fieldAccess.fieldSelection.field
      val baseIndex = fieldAccess.index
      val level = field.level

      var a0 : (() => Expression) = (() => { NullExpression })
      var a1 : (() => Expression) = (() => { NullExpression })
      var x0 = (() => { Duplicate(fieldAccess) })
      var x1 = (() => { offsetAccess(fieldAccess, 1, faceDim) })

      field.discretization match {
        case "cell" => {
          if (stagDim.isDefined) {
            return fieldAccess // value is located at the evaluation region
          } else {
            a0 = (() => { cellCenterToFace(level, Duplicate(baseIndex), None, faceDim) })
            a1 = (() => { cellCenterToFace(level, offsetIndex(baseIndex, 1, faceDim), None, faceDim) })
          }
        }
        case "face_x" | "face_y" | "face_z" => {
          if (stagDim.isDefined) {
            if (s"face_${dimToString(faceDim)}" == field.discretization) {
              // interpolation weights are always 0.5 due to geometric construction
              a0 = (() => { 0.5 })
              a1 = (() => { 0.5 })
            } else {
              Logger.warn(s"Trying to evaluate $fieldAccess on face dimension $faceDim of a ${stagDim.get} staggered CV. This is not unique. Defaulting to leftmost candidate")
              return offsetAccess(fieldAccess, 1, stagDim.get)
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
        case _ =>
          Logger.warn(s"Trying to use interpolation scheme $interpolation which is unknown - falling back to default scheme")
          EvalAtRFace(fieldAccess, faceDim, stagDim, "default").expandSpecial
      }
    }
  }

  // integrations
  def integrateOverEastFace(exp : Expression) : Expression = integrateOverRFace(exp, 0, None)
  def integrateOverWestFace(exp : Expression) : Expression = integrateOverLFace(exp, 0, None)
  def integrateOverNorthFace(exp : Expression) : Expression = integrateOverRFace(exp, 1, None)
  def integrateOverSouthFace(exp : Expression) : Expression = integrateOverLFace(exp, 1, None)
  def integrateOverTopFace(exp : Expression) : Expression = integrateOverRFace(exp, 2, None)
  def integrateOverBottomFace(exp : Expression) : Expression = integrateOverLFace(exp, 2, None)

  def integrateOverXStaggeredEastFace(exp : Expression) : Expression = integrateOverRFace(exp, 0, Some(0))
  def integrateOverXStaggeredWestFace(exp : Expression) : Expression = integrateOverLFace(exp, 0, Some(0))
  def integrateOverXStaggeredNorthFace(exp : Expression) : Expression = integrateOverRFace(exp, 1, Some(0))
  def integrateOverXStaggeredSouthFace(exp : Expression) : Expression = integrateOverLFace(exp, 0, Some(1))
  def integrateOverXStaggeredTopFace(exp : Expression) : Expression = integrateOverRFace(exp, 2, Some(0))
  def integrateOverXStaggeredBottomFace(exp : Expression) : Expression = integrateOverLFace(exp, 0, Some(2))

  def integrateOverYStaggeredEastFace(exp : Expression) : Expression = integrateOverRFace(exp, 0, Some(1))
  def integrateOverYStaggeredWestFace(exp : Expression) : Expression = integrateOverLFace(exp, 1, Some(0))
  def integrateOverYStaggeredNorthFace(exp : Expression) : Expression = integrateOverRFace(exp, 1, Some(1))
  def integrateOverYStaggeredSouthFace(exp : Expression) : Expression = integrateOverLFace(exp, 1, Some(1))
  def integrateOverYStaggeredTopFace(exp : Expression) : Expression = integrateOverRFace(exp, 2, Some(1))
  def integrateOverYStaggeredBottomFace(exp : Expression) : Expression = integrateOverLFace(exp, 1, Some(2))

  def integrateOverZStaggeredEastFace(exp : Expression) : Expression = integrateOverRFace(exp, 0, Some(2))
  def integrateOverZStaggeredWestFace(exp : Expression) : Expression = integrateOverLFace(exp, 2, Some(0))
  def integrateOverZStaggeredNorthFace(exp : Expression) : Expression = integrateOverRFace(exp, 1, Some(2))
  def integrateOverZStaggeredSouthFace(exp : Expression) : Expression = integrateOverLFace(exp, 2, Some(1))
  def integrateOverZStaggeredTopFace(exp : Expression) : Expression = integrateOverRFace(exp, 2, Some(2))
  def integrateOverZStaggeredBottomFace(exp : Expression) : Expression = integrateOverLFace(exp, 2, Some(2))

  def integrateOverLFace(exp : Expression, faceDim : Int, stagDim : Option[Int]) : Expression = {
    ShiftFieldAccessIndices.offset = -1
    ShiftFieldAccessIndices.dim = faceDim
    ShiftFieldAccessIndices.applyStandalone(exp)

    integrateOverRFace(exp, faceDim, stagDim)
  }

  // integration over faces of staggered CVs is done by defining stagDim - the dimension in which the grid is staggered
  def integrateOverRFace(exp : Expression, faceDim : Int, stagDim : Option[Int]) : Expression = {
    // check if there are any field accesses in the current (sub-)expression
    CollectFieldAccesses.applyStandalone(new Scope(exp))

    // TODO: find a way to handle constants
    if (0 == CollectFieldAccesses.fieldAccesses.size) {
      Logger.warn(s"Trying to evaluate index-independent expression ${exp.prettyprint} - currently unsupported")
      return exp
    }

    // check if all occurring level specifications are identical
    val level = CollectFieldAccesses.fieldAccesses(0).fieldSelection.level
    for (fieldAccess <- CollectFieldAccesses.fieldAccesses) {
      if (level != fieldAccess.fieldSelection.level) {
        Logger.warn(s"Mixed level field integration is currently not supported (${exp.prettyprint})")
        return exp
      }
    }
    for (fieldAccess <- CollectFieldAccesses.vFieldAccesses) { // check virtual fields as well
      if (level != fieldAccess.level) {
        Logger.warn(s"Mixed level field integration is currently not supported (${exp.prettyprint})")
        return exp
      }
    }

    // step 1: wrap field accesses with eval functions if necessary
    object WrappingFieldAccesses extends QuietDefaultStrategy("Wrapping field accesses") {
      val pIntAnnot = Annotation("PIECEWISE_INTEGRATION")
      def addPIntAnnot(exp : Expression) = { exp.add(pIntAnnot); exp }

      this += new Transformation("Wrapping", {
        case fieldAccess : FieldAccess => {
          val discr = fieldAccess.fieldSelection.field.discretization
          if (stagDim.isDefined) {
            val curStagDim = stagDim.get
            discr match {
              case "cell" if curStagDim == faceDim => fieldAccess // direct sampling
              case "cell" if curStagDim != faceDim => addPIntAnnot(EvalAtRFace(offsetAccess(fieldAccess, -1, curStagDim), faceDim, stagDim)) // interpolation with offset, piecewiseIntegration
              case fieldDiscr @ ("face_x" | "face_y" | "face_z") if (s"face_${dimToString(curStagDim)}" == fieldDiscr) => // field discretization matches CV
                EvalAtRFace(fieldAccess, faceDim, stagDim) // interpolation
              case "face_x" if 0 != curStagDim => // -1 in CV's stag dim and +1 in field discretization's stag dim
                addPIntAnnot(offsetAccess(offsetAccess(fieldAccess, -1, curStagDim), 1, 0)) // direct sampling with offset, piecewiseIntegration
              case "face_y" if 1 != curStagDim => // -1 in CV's stag dim and +1 in field discretization's stag dim
                addPIntAnnot(offsetAccess(offsetAccess(fieldAccess, -1, curStagDim), 1, 1)) // direct sampling with offset, piecewiseIntegration
              case "face_z" if 2 != curStagDim => // -1 in CV's stag dim and +1 in field discretization's stag dim
                addPIntAnnot(offsetAccess(offsetAccess(fieldAccess, -1, curStagDim), 1, 2)) // direct sampling with offset, piecewiseIntegration
              case _ => Logger.error(s"Unknown or unsupported discretization $discr for field $fieldAccess in staggered integration")
            }
          } else {
            discr match {
              case "cell" => EvalAtRFace(fieldAccess, faceDim, stagDim) // interpolation
              case "face_x" | "face_y" | "face_z" => {
                if (s"face_${dimToString(faceDim)}" == discr)
                  offsetAccess(fieldAccess, 1, faceDim) // direct sampling with offset
                else
                  addPIntAnnot(EvalAtRFace(fieldAccess, faceDim, stagDim)) // interpolation, piecewiseIntegration
              }
              case _ => Logger.error(s"Unknown or unsupported discretization $discr for field $fieldAccess in basic integration")
            }
          }
        }
        // TODO: case VirtualFieldAccess
        case eval : EvalAtRFace => {
          if (eval.faceDim != faceDim) Logger.error(s"Found unaligned eval for faceDim ${eval.faceDim} in integration for faceDim $faceDim in eval for ${eval.fieldAccess}")
          if (eval.stagDim != stagDim) Logger.error(s"Found unaligned eval for stagDim ${eval.stagDim} in integration for stagDim $stagDim in eval for ${eval.fieldAccess}")

          // TODO: summarize
          val discr = eval.fieldAccess.fieldSelection.field.discretization
          if (stagDim.isDefined) {
            val curStagDim = stagDim.get
            discr match {
              case "cell" if curStagDim == faceDim => eval.fieldAccess // direct sampling
              case "cell" if curStagDim != faceDim =>
                eval.fieldAccess = offsetAccess(eval.fieldAccess, -1, curStagDim)
                addPIntAnnot(eval) // interpolation with offset, piecewiseIntegration
              case fieldDiscr @ ("face_x" | "face_y" | "face_z") if (s"face_${dimToString(curStagDim)}" == fieldDiscr) => // field discretization matches CV
                eval // orig eval is fine
              case "face_x" if 0 != curStagDim => // -1 in CV's stag dim and +1 in field discretization's stag dim => ignore eval as direct sampling is possible
                addPIntAnnot(offsetAccess(offsetAccess(eval.fieldAccess, -1, curStagDim), 1, 0)) // direct sampling with offset, piecewiseIntegration
              case "face_y" if 1 != curStagDim =>
                addPIntAnnot(offsetAccess(offsetAccess(eval.fieldAccess, -1, curStagDim), 1, 1))
              case "face_z" if 2 != curStagDim =>
                addPIntAnnot(offsetAccess(offsetAccess(eval.fieldAccess, -1, curStagDim), 1, 2))
              case _ => Logger.error(s"Unknown or unsupported discretization $discr for field ${eval.fieldAccess} in staggered integration")
            }
          } else {
            discr match {
              case "cell" => eval // interpolation
              case "face_x" | "face_y" | "face_z" => {
                if (s"face_${dimToString(faceDim)}" == discr) {
                  eval.fieldAccess = offsetAccess(eval.fieldAccess, 1, faceDim)
                  eval // direct sampling with offset
                } else
                  addPIntAnnot(eval) // interpolation, piecewiseIntegration
              }
              case _ => Logger.error(s"Unknown or unsupported discretization $discr for field ${eval.fieldAccess} in basic integration")
            }
          }
        }
        case fctCall @ FunctionCallExpression(functionName, args) if ResolveGeometryFunctions.integrateFunctions.contains(functionName) => {
          Logger.error("Integration functions called inside other integration functions are currently not supported")
        }
      }, false) // not recursive -> don't look inside eval functions
    }
    WrappingFieldAccesses.applyStandalone(new Scope(exp))

    // step 2: check if integration by parts is required
    var piecewiseIntegration = StateManager.findFirst({ n : Node => n.hasAnnotation(WrappingFieldAccesses.pIntAnnot) }, new Scope(exp)).isDefined

    // step 3: apply chosen integration

    object ShiftFieldAccessIndices_ extends QuietDefaultStrategy("Shifting indices of field accesses") {
      var offset : Expression = 0
      var dim : Int = 0
      var requiredAnnot : Option[Annotation] = None

      this += new Transformation("Searching and shifting", {
        case fieldAccess : FieldAccess if requiredAnnot.isEmpty || fieldAccess.hasAnnotation(requiredAnnot.get) =>
          fieldAccess.index(dim) += offset
          fieldAccess
        case fieldAccess : VirtualFieldAccess if requiredAnnot.isEmpty || fieldAccess.hasAnnotation(requiredAnnot.get) =>
          fieldAccess.index(dim) += offset
          fieldAccess
        case eval : EvalAtRFace if requiredAnnot.isEmpty || eval.hasAnnotation(requiredAnnot.get) =>
          eval.fieldAccess.index(dim) += offset
          eval
      }, false) // skip field accesses inside eval nodes
    }

    if (piecewiseIntegration) {
      def index = LoopOverDimensions.defIt

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
        ShiftFieldAccessIndices_.applyStandalone(offsetExp)

        (VirtualFieldAccess(s"vf_cellWidth_${dimToString(compDim)}", level, index) *
          (VirtualFieldAccess(s"vf_cellCenterToFace_${dimToString(curStagDim)}", level, offsetIndex(index, -1, curStagDim)) * centerExp
            + VirtualFieldAccess(s"vf_cellCenterToFace_${dimToString(curStagDim)}", level, index) * offsetExp))
      } else {
        Logger.error("piecewise integration on non-staggered cell interfaces is not supported")
      }
    } else {
      def index = LoopOverDimensions.defIt

      if (stagDim.isDefined) {
        val curStagDim = stagDim.get

        if (curStagDim == faceDim) {
          val compDim0 = (if (0 == faceDim) 1 else 0)
          val compDim1 = (if (2 == faceDim) 1 else 2)

          cellWidth(level, index, None, compDim0) * cellWidth(level, index, None, compDim1) * exp
        } else {
          val compDim = (if (0 != faceDim && 0 != curStagDim) 0 else (if (1 != faceDim && 1 != curStagDim) 1 else 2))

          cellWidth(level, index, None, compDim) * stagCVWidth(level, index, None, curStagDim) * exp
        }
      } else {
        val compDim0 = (if (0 == faceDim) 1 else 0)
        val compDim1 = (if (2 == faceDim) 1 else 2)

        cellWidth(level, index, None, compDim0) * cellWidth(level, index, None, compDim1) * exp
      }
    }

    /*
    // filter cases in which only one field access happens
    if (1 == CollectFieldAccesses.fieldAccesses.size) {
      val index = (() => { Duplicate(CollectFieldAccesses.fieldAccesses(0).index) })
      val discr = CollectFieldAccesses.fieldAccesses(0).fieldSelection.field.discretization

      return discr match {
        case "cell" if !stagDim.isDefined => { // integration of cell values on non-staggered CVs comes down to interpolation
          val compDim0 = (if (0 == faceDim) 1 else 0)
          val compDim1 = (if (2 == faceDim) 1 else 2)

          ReplaceFieldAccesses.replacement = EvalAtRFace(CollectFieldAccesses.fieldAccesses(0), faceDim, stagDim)
          ReplaceFieldAccesses.applyStandalone(exp)

          cellWidth(level, index(), None, compDim0) * cellWidth(level, index(), None, compDim1) * exp
        }

        case "cell" if stagDim.isDefined => { // value is defined on the interface
          val curStagDim = stagDim.get
          if (curStagDim == faceDim) { // evaluation of a cell centered value w.r.t. a staggered face (in the correct dim) is simply sampling
            val compDim0 = (if (0 == faceDim) 1 else 0)
            val compDim1 = (if (2 == faceDim) 1 else 2)

            (VirtualFieldAccess(s"vf_cellWidth_${dimToString(compDim0)}", level, index())
              * VirtualFieldAccess(s"vf_cellWidth_${dimToString(compDim1)}", level, index())
              * exp)
          } else { // stagDim != faceDim => piecewise integration is required
            val compDim = (if (0 != faceDim && 0 != curStagDim) 0 else (if (1 != faceDim && 1 != curStagDim) 1 else 2))

            val centerExp = Duplicate(exp)
            val offsetExp = Duplicate(exp)

            ShiftFieldAccessIndices.offset = -1
            ShiftFieldAccessIndices.dim = curStagDim
            ShiftFieldAccessIndices.applyStandalone(offsetExp)

            (VirtualFieldAccess(s"vf_cellWidth_${dimToString(compDim)}", level, index()) *
              (VirtualFieldAccess(s"vf_cellCenterToFace_${dimToString(curStagDim)}", level, index()) * centerExp
                + VirtualFieldAccess(s"vf_cellCenterToFace_${dimToString(curStagDim)}", level, offsetIndex(index(), -1, curStagDim)) * offsetExp))
          }
        }

        case _ =>
          Logger.warn(s"Integration over staggered faces for expression ${exp.prettyprint} is currently not supported")
          exp
      }
    }

    // more complicated case => match current expression and process it in parts
    return exp match {
      case AdditionExpression(left, right) => // addition components can be integrated separately
        integrateOverRFace(left, faceDim, stagDim) + integrateOverRFace(right, faceDim, stagDim)

      case MultiplicationExpression(left : Number, right) => // resolve multiplications with constants
        left * integrateOverRFace(right, faceDim, stagDim)
      case MultiplicationExpression(left, right : Number) => // resolve multiplications with constants
        right * integrateOverRFace(left, faceDim, stagDim)

      case MultiplicationExpression(left : FieldAccess, right : FieldAccess) if (
        stagDim.isDefined
        && s"face_${dimToString(faceDim)}" == left.fieldSelection.field.discretization
        && "cell" == right.fieldSelection.field.discretization) => {

        val index = (() => Duplicate(right.index))
        val curStagDim = stagDim.get

        if (faceDim == curStagDim) {
          val compDim0 = (if (0 == faceDim) 1 else 0)
          val compDim1 = (if (2 == faceDim) 1 else 2)

          (VirtualFieldAccess(s"vf_cellWidth_${dimToString(compDim0)}", level, index())
            * VirtualFieldAccess(s"vf_cellWidth_${dimToString(compDim1)}", level, index())
            * 0.5 * (Duplicate(left) + offsetAccess(left, 1, faceDim)) * Duplicate(right))
        } else { // faceDim != stagDim
          val compDim = (if (0 != faceDim && 0 != curStagDim) 0 else (if (1 != faceDim && 1 != curStagDim) 1 else 2))

          // eval cell value at face points; multiply with face values; multiply with dist to (original) cell interface; add up
          (VirtualFieldAccess(s"vf_cellWidth_${dimToString(compDim)}", level, index()) *
            ((VirtualFieldAccess(s"vf_cellCenterToFace_${dimToString(curStagDim)}", level, index())
              * offsetAccess(left, 1, faceDim)
              * EvalAtRFace(Duplicate(right), faceDim, stagDim))
              + (VirtualFieldAccess(s"vf_cellCenterToFace_${dimToString(curStagDim)}", level, offsetIndex(index(), -1, curStagDim))
                * offsetAccess(offsetAccess(left, -1, curStagDim), 1, faceDim)
                * EvalAtRFace(offsetAccess(right, -1, curStagDim), faceDim, stagDim))))
        }
      }

      case MultiplicationExpression(left : FieldAccess, right : FieldAccess) if (
        !stagDim.isDefined
        && "cell" == right.fieldSelection.field.discretization
        && s"face_${dimToString(faceDim)}" == left.fieldSelection.field.discretization) => {
        val index = (() => Duplicate(right.index))

        val compDim0 = (if (0 == faceDim) 1 else 0)
        val compDim1 = (if (2 == faceDim) 1 else 2)

        val rightIndex = right.index
        (cellWidth(level, rightIndex, None, compDim0) * cellWidth(level, rightIndex, None, compDim1)
          * offsetAccess(left, 1, faceDim) * EvalAtRFace(right, faceDim, stagDim))
      }

      case MultiplicationExpression(left : FieldAccess, right : FieldAccess) if (
        //(stagDim.isDefined || !stagDim.isDefined) &&
        "cell" == right.fieldSelection.field.discretization
        && s"face_${dimToString(faceDim)}" == left.fieldSelection.field.discretization) => {
        integrateOverRFace(MultiplicationExpression(right, left), faceDim, stagDim)
      }

      case _ => {
        Logger.warn(s"Integration over staggered faces for expression ${exp.prettyprint} is currently not supported")
        exp
      }
    }*/
  }
}
