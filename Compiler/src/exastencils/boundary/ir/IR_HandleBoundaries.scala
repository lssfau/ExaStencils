package exastencils.boundary.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.deprecated.ir._
import exastencils.domain.ir._
import exastencils.field.ir.IR_FieldAccess
import exastencils.grid.ir._
import exastencils.logger._
import exastencils.parallelization.ir.IR_ParallelizationInfo

/// IR_HandleBoundaries

// TODO: refactor
case class IR_HandleBoundaries(var field : IR_FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)]) extends IR_Statement with IR_Expandable {
  // TODO: inline after fully integrating experimental_internalHighDimTypes functionality
  def numDims = if (!Knowledge.experimental_internalHighDimTypes) field.fieldLayout.numDimsData else field.fieldLayout.numDimsGrid

  def setupFieldUpdate(neigh : NeighborInfo) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // apply local trafo and replace boundaryCoord
    val strat = QuietDefaultStrategy("ResolveBoundaryCoordinates")
    strat += new Transformation("SearchAndReplace", {
      case virtualField @ IR_VirtualFieldAccess(target : IR_VF_BoundaryPositionPerDim, index, fragIdx) =>
        val evalDim = target.dim

        field.fieldLayout.localization match {
          // TODO: adapt for grids that are not axis-parallel
          case IR_AtNode | IR_AtFaceCenter(`evalDim`) =>
            virtualField.target = IR_VF_NodePositionPerDim.find(target.level, evalDim)

          case IR_AtCellCenter | IR_AtFaceCenter(_) =>
            if (0 == neigh.dir(evalDim)) { // simple projection
              virtualField.target = IR_VF_CellCenterPerDim.find(target.level, evalDim)
            } else if (neigh.dir(evalDim) < 0) { // snap to left boundary
              virtualField.target = IR_VF_NodePositionPerDim.find(target.level, evalDim)
            } else { // snap to right boundary
              virtualField.target = IR_VF_NodePositionPerDim.find(target.level, evalDim)
              virtualField.index = IR_GridUtil.offsetIndex(virtualField.index, 1, evalDim)
            }
        }
        virtualField
      //Grid.getGridObject.invokeAccessResolve(virtualField)
    })

    val bc = Duplicate(field.field.boundary)
    strat.applyStandalone(IR_Root(bc))

    // FIXME: this works for now, but users may want to specify bc's per vector element
    // FIXME: (update) adapt for numDimsGrid once new vector and matrix data types are fully integrated
    val index = IR_LoopOverDimensions.defIt(numDims)
    def fieldSel = Duplicate(IR_FieldSelection(field.field, field.level, field.slot, field.fragIdx)) // TODO: check

    def offsetIndex = IR_ExpressionIndex(neigh.dir ++ Array.fill(numDims - field.fieldLayout.numDimsGrid)(0))
    def offsetIndexWithTrafo(f : (Int => Int)) = IR_ExpressionIndex(neigh.dir.map(f) ++ Array.fill(numDims - field.fieldLayout.numDimsGrid)(0))

    bc match {
      case IR_NeumannBC(order) =>
        // TODO: move this logic to the appropriate bc classes

        def forNode() = order match {
          case 1 => statements += IR_Assignment(IR_FieldAccess(fieldSel, index), IR_FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -i)))
          case 2 => statements += IR_Assignment(IR_FieldAccess(fieldSel, index),
            ((4.0 / 3.0) * IR_FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -i)))
              + ((-1.0 / 3.0) * IR_FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -2 * i))))
          case 3 => // TODO: do we want this? what do we do on the coarser levels?
            statements += IR_Assignment(IR_FieldAccess(fieldSel, index),
              ((3.0 * 6.0 / 11.0) * IR_FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -1 * i)))
                + ((-3.0 / 2.0 * 6.0 / 11.0) * IR_FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -2 * i)))
                + ((1.0 / 3.0 * 6.0 / 11.0) * IR_FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -3 * i))))
        }

        def forCell() = order match {
          case 1 => statements += IR_Assignment(IR_FieldAccess(fieldSel, index + offsetIndex), IR_FieldAccess(fieldSel, index))
        }

        field.fieldLayout.localization match {
          case IR_AtNode                                           => forNode()
          case IR_AtFaceCenter(faceDim) if 0 != neigh.dir(faceDim) => forNode()
          case IR_AtCellCenter                                     => forCell()
          case IR_AtFaceCenter(_)                                  => forCell()
        }

      case IR_DirichletBC(boundaryExpr, order) =>
        def forNode() = statements += IR_Assignment(IR_FieldAccess(fieldSel, index), boundaryExpr)

        def forCell() = order match {
          case 1 => statements += IR_Assignment(IR_FieldAccess(fieldSel, index + offsetIndex),
            (2.0 * boundaryExpr) - IR_FieldAccess(fieldSel, index))
          case 2 =>
            // determine weights for interpolation
            var w_0_5, w_1, w_2 : IR_Expression = 0
            if (Knowledge.grid_isUniform) {
              w_0_5 = 8.0 / 3.0
              w_1 = -2
              w_2 = 1.0 / 3.0
            } else {
              // non-linear => use Lagrange polynomials
              val nonZero = neigh.dir.zipWithIndex.filter(_._1 != 0)
              if (nonZero.length != 1) Logger.error("Malformed neighbor index vector " + neigh.dir.mkString(", "))
              val dim = nonZero(0)._2
              def x = IR_VF_CellCenterPerDim.access(field.level, dim, index + offsetIndex)
              def x_0_5 =
                if (neigh.dir(dim) > 0)
                  IR_VF_NodePositionPerDim.access(field.level, dim, index + offsetIndex)
                else
                  IR_VF_NodePositionPerDim.access(field.level, dim, index)
              def x_1 = IR_VF_CellCenterPerDim.access(field.level, dim, index)
              def x_2 = IR_VF_CellCenterPerDim.access(field.level, dim, index + offsetIndexWithTrafo(i => -i))
              w_0_5 = ((x - x_1) * (x - x_2)) / ((x_0_5 - x_1) * (x_0_5 - x_2))
              w_1 = ((x - x_0_5) * (x - x_2)) / ((x_1 - x_0_5) * (x_1 - x_2))
              w_2 = ((x - x_0_5) * (x - x_1)) / ((x_2 - x_0_5) * (x_2 - x_1))
            }
            statements += IR_Assignment(IR_FieldAccess(fieldSel, index + offsetIndex),
              w_0_5 * boundaryExpr + w_1 * IR_FieldAccess(fieldSel, index) + w_2 * IR_FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -i)))
        }

        field.fieldLayout.localization match {
          case IR_AtNode                                           => forNode()
          case IR_AtFaceCenter(faceDim) if 0 != neigh.dir(faceDim) => forNode()
          case IR_AtCellCenter                                     => forCell()
          case IR_AtFaceCenter(_)                                  => forCell()
        }
    }

    statements
  }

  def constructLoops = {
    val layout = field.field.fieldLayout

    IR_LoopOverFragments(
      ListBuffer[IR_Statement](IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
        neighbors.map({ neigh =>
          val adaptedIndexRange = IR_ExpressionIndexRange(neigh._2.begin - field.referenceOffset, neigh._2.end - field.referenceOffset)
          // TODO: assumes equal bc's for all components
          adaptedIndexRange.begin.indices ++= (layout.numDimsGrid until numDims).map(dim => 0 : IR_Expression)
          adaptedIndexRange.end.indices ++= (layout.numDimsGrid until numDims).map(dim => layout.idxById("TOT", dim))
          val loopOverDims = new IR_LoopOverDimensions(
            numDims,
            adaptedIndexRange,
            setupFieldUpdate(neigh._1))
          loopOverDims.parallelization.potentiallyParallel = true
          loopOverDims.polyOptLevel = 1
          IR_IfCondition(IR_Negation(IR_IV_NeighborIsValid(field.domainIndex, neigh._1.index)), loopOverDims) : IR_Statement
        }))), IR_ParallelizationInfo.PotentiallyParallel())
  }

  override def expand() : Output[IR_Statement] = {
    field.field.boundary match {
      case _ : IR_NeumannBC                => constructLoops
      case _ : IR_DirichletBC              => constructLoops
      case IR_FunctionBC(boundaryFunction) => boundaryFunction : IR_Statement
      case IR_NoBC                         => IR_NullStatement
    }
  }
}
