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
import exastencils.grid._
import exastencils.grid.ir.IR_VirtualFieldAccess
import exastencils.logger._
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.polyhedron.PolyhedronAccessible

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
      case virtualField : IR_VirtualFieldAccess if virtualField.fieldName.startsWith("boundaryCoord") || virtualField.fieldName.startsWith("vf_boundaryCoord") =>
        val evalDim = virtualField.fieldName match {
          case "boundaryCoord_x" | "vf_boundaryCoord_x" => 0
          case "boundaryCoord_y" | "vf_boundaryCoord_y" => 1
          case "boundaryCoord_z" | "vf_boundaryCoord_z" => 2
          case _                                        => Logger.error(s"Invalid virtual field named ${ virtualField.fieldName } found")
        }

        field.fieldLayout.discretization match {
          // TODO: adapt for grids that are not axis-parallel
          case discr if "node" == discr || ("face_x" == discr && 0 == evalDim) || ("face_y" == discr && 1 == evalDim) || ("face_z" == discr && 2 == evalDim) =>
            virtualField.fieldName = virtualField.fieldName.replace("boundaryCoord", "nodePosition")

          case discr if "cell" == discr || ("face_x" == discr && 0 != evalDim) || ("face_y" == discr && 1 != evalDim) || ("face_z" == discr && 2 != evalDim) =>
            if (0 == neigh.dir(evalDim)) { // simple projection
              virtualField.fieldName = virtualField.fieldName.replace("boundaryCoord", "cellCenter")
            } else if (neigh.dir(evalDim) < 0) { // snap to left boundary
              virtualField.fieldName = virtualField.fieldName.replace("boundaryCoord", "nodePosition")
            } else { // snap to right boundary
              virtualField.fieldName = virtualField.fieldName.replace("boundaryCoord", "nodePosition")
              virtualField.index = GridUtil.offsetIndex(virtualField.index, 1, evalDim)
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
    def fieldSel = Duplicate(IR_FieldSelection(field.field, field.level, field.slot, None, field.fragIdx)) // TODO: check

    def offsetIndex = IR_ExpressionIndex(neigh.dir ++ Array.fill(numDims - field.fieldLayout.numDimsGrid)(0))
    def offsetIndexWithTrafo(f : (Int => Int)) = IR_ExpressionIndex(neigh.dir.map(f) ++ Array.fill(numDims - field.fieldLayout.numDimsGrid)(0))

    bc match {
      case IR_NeumannBC(order)                 =>
        // TODO: move this logic to the appropriate bc classes
        field.fieldLayout.discretization match {
          case d if "node" == d
            || ("face_x" == d && 0 != neigh.dir(0))
            || ("face_y" == d && 0 != neigh.dir(1))
            || ("face_z" == d && 0 != neigh.dir(2)) =>
            order match {
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
          case d if "cell" == d
            || ("face_x" == d && 0 == neigh.dir(0))
            || ("face_y" == d && 0 == neigh.dir(1))
            || ("face_z" == d && 0 == neigh.dir(2)) =>
            order match {
              case 1 => statements += IR_Assignment(IR_FieldAccess(fieldSel, index + offsetIndex), IR_FieldAccess(fieldSel, index))
            }
        }
      case IR_DirichletBC(boundaryExpr, order) =>
        field.fieldLayout.discretization match {
          case d if "node" == d
            || ("face_x" == d && 0 != neigh.dir(0))
            || ("face_y" == d && 0 != neigh.dir(1))
            || ("face_z" == d && 0 != neigh.dir(2)) =>
            statements += IR_Assignment(IR_FieldAccess(fieldSel, index), boundaryExpr)
          case d if "cell" == d
            || ("face_x" == d && 0 == neigh.dir(0))
            || ("face_y" == d && 0 == neigh.dir(1))
            || ("face_z" == d && 0 == neigh.dir(2)) =>
            order match {
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
                  def x = IR_VirtualFieldAccess(s"vf_cellCenter_${ IR_DimToString(dim) }", field.level, index + offsetIndex)
                  def x_0_5 =
                    if (neigh.dir(dim) > 0)
                      IR_VirtualFieldAccess(s"vf_nodePosition_${ IR_DimToString(dim) }", field.level, index + offsetIndex)
                    else
                      IR_VirtualFieldAccess(s"vf_nodePosition_${ IR_DimToString(dim) }", field.level, index)
                  def x_1 = IR_VirtualFieldAccess(s"vf_cellCenter_${ IR_DimToString(dim) }", field.level, index)
                  def x_2 = IR_VirtualFieldAccess(s"vf_cellCenter_${ IR_DimToString(dim) }", field.level, index + offsetIndexWithTrafo(i => -i))
                  w_0_5 = ((x - x_1) * (x - x_2)) / ((x_0_5 - x_1) * (x_0_5 - x_2))
                  w_1 = ((x - x_0_5) * (x - x_2)) / ((x_1 - x_0_5) * (x_1 - x_2))
                  w_2 = ((x - x_0_5) * (x - x_1)) / ((x_2 - x_0_5) * (x_2 - x_1))
                }
                statements += IR_Assignment(IR_FieldAccess(fieldSel, index + offsetIndex),
                  w_0_5 * boundaryExpr + w_1 * IR_FieldAccess(fieldSel, index) + w_2 * IR_FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -i)))
            }
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
            setupFieldUpdate(neigh._1)) with PolyhedronAccessible
          loopOverDims.parallelization.potentiallyParallel = true
          loopOverDims.optLevel = 1
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
