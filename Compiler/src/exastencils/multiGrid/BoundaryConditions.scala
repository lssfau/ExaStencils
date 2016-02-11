package exastencils.multiGrid

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.grid._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.omp._
import exastencils.polyhedron._
import exastencils.prettyprinting._

case class HandleBoundaries(var field : FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = HandleBoundaries\n"

  def setupFieldUpdate(neigh : NeighborInfo) : ListBuffer[Statement] = {
    var statements : ListBuffer[Statement] = ListBuffer()

    // apply local trafo and replace boundaryCoord
    val strat = QuietDefaultStrategy("ResolveBoundaryCoordinates")
    strat += new Transformation("SearchAndReplace", {
      case virtualField : VirtualFieldAccess if virtualField.fieldName.startsWith("boundaryCoord") || virtualField.fieldName.startsWith("vf_boundaryCoord") => {
        val evalDim = virtualField.fieldName match {
          case "boundaryCoord_x" | "vf_boundaryCoord_x" => 0
          case "boundaryCoord_y" | "vf_boundaryCoord_y" => 1
          case "boundaryCoord_z" | "vf_boundaryCoord_z" => 2
          case _                                        => Logger.error(s"Invalid virtual field named ${virtualField.fieldName} found")
        }

        field.fieldLayout.discretization match {
          // TODO: adapt for grids that are not axis-parallel
          case "node" => virtualField.fieldName = virtualField.fieldName.replace("boundaryCoord", "nodePosition")
          //case d if         || ("face_x" == d && 0 != neigh.dir(0))         || ("face_y" == d && 0 != neigh.dir(1))         || ("face_z" == d && 0 != neigh.dir(2))

          case "cell" => {
            if (0 == neigh.dir(evalDim)) { // simple projection
              virtualField.fieldName = virtualField.fieldName.replace("boundaryCoord", "cellCenter")
            } else if (neigh.dir(evalDim) < 0) { // snap to left boundary
              virtualField.fieldName = virtualField.fieldName.replace("boundaryCoord", "nodePosition")
            } else { // snap to right boundary
              virtualField.fieldName = virtualField.fieldName.replace("boundaryCoord", "nodePosition")
              virtualField.index = GridUtil.offsetIndex(virtualField.index, 1, evalDim)
            }
          }
        }
        virtualField
        //Grid.getGridObject.invokeAccessResolve(virtualField)
      }
    })
    val bc = Duplicate(field.field.boundaryConditions.get)
    strat.applyStandalone(ExpressionStatement(bc))

    // FIXME: this works for now, but users may want to specify bc's per vector element
    // FIXME: (update) adapt for numDimsGrid once new vector and matrix data types are fully integrated
    var index = LoopOverDimensions.defIt(field.fieldLayout.numDimsData)
    var fieldSel = new FieldSelection(field.field, field.level, field.slot, None, field.fragIdx) // TODO: check

    def offsetIndex = new MultiIndex(neigh.dir ++ Array.fill(field.fieldLayout.numDimsData - field.fieldLayout.numDimsGrid)(0))
    def offsetIndexWithTrafo(f : (Int => Int)) = new MultiIndex(neigh.dir.map(f) ++ Array.fill(field.fieldLayout.numDimsData - field.fieldLayout.numDimsGrid)(0))

    field.fieldLayout.discretization match {
      case d if "node" == d
        || ("face_x" == d && 0 != neigh.dir(0))
        || ("face_y" == d && 0 != neigh.dir(1))
        || ("face_z" == d && 0 != neigh.dir(2)) =>
        if (StringLiteral("Neumann") == bc)
          Knowledge.experimental_NeumannOrder match {
            case 1 => statements += new AssignmentStatement(new FieldAccess(fieldSel, index), new FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -i)))
            case 2 => statements += new AssignmentStatement(new FieldAccess(fieldSel, index),
              ((4.0 / 3.0) * new FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -i)))
                + ((-1.0 / 3.0) * new FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -2 * i))))
            case 3 => // TODO: do we want this? what do we do on the coarser levels?
              statements += new AssignmentStatement(new FieldAccess(fieldSel, index),
                (((3.0 * 6.0 / 11.0) * new FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -1 * i)))
                  + ((-3.0 / 2.0 * 6.0 / 11.0) * new FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -2 * i)))
                  + ((1.0 / 3.0 * 6.0 / 11.0) * new FieldAccess(fieldSel, index + offsetIndexWithTrafo(i => -3 * i)))))
          }
        else
          statements += new AssignmentStatement(new FieldAccess(fieldSel, index), bc)
      case d if "cell" == d
        || ("face_x" == d && 0 == neigh.dir(0))
        || ("face_y" == d && 0 == neigh.dir(1))
        || ("face_z" == d && 0 == neigh.dir(2)) =>
        if (StringLiteral("Neumann") == bc)
          Knowledge.experimental_NeumannOrder match {
            case 1 => statements += new AssignmentStatement(new FieldAccess(fieldSel, index + offsetIndex),
              new FieldAccess(fieldSel, index))
          }
        else
          statements += new AssignmentStatement(new FieldAccess(fieldSel, index + offsetIndex),
            (2.0 * bc) - new FieldAccess(fieldSel, index))
    }

    statements
  }

  override def expand : Output[Statement] = {
    if (field.field.boundaryConditions.isDefined) {
      new LoopOverFragments(
        new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
          neighbors.map({ neigh =>
            val adaptedIndexRange = IndexRange(neigh._2.begin - field.referenceOffset, neigh._2.end - field.referenceOffset)
            val loopOverDims = new LoopOverDimensions(
              field.fieldLayout.numDimsData,
              adaptedIndexRange,
              setupFieldUpdate(neigh._1)) with OMP_PotentiallyParallel with PolyhedronAccessable
            loopOverDims.optLevel = 1
            new ConditionStatement(NegationExpression(iv.NeighborIsValid(field.domainIndex, neigh._1.index)), loopOverDims) : Statement
          }))) with OMP_PotentiallyParallel
    } else {
      NullStatement
    }
  }
}
