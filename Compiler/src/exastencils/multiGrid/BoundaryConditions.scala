package exastencils.multiGrid

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.omp._
import exastencils.polyhedron._
import exastencils.prettyprinting._

case class HandleBoundaries(var field : FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = HandleBoundaries\n"

  def setupFieldUpdate(neigh : NeighborInfo) : ListBuffer[Statement] = {
    var statements : ListBuffer[Statement] = ListBuffer()
    if (StateManager.findFirst[AnyRef]((node : Any) => node match {
      case StringConstant("xPos") | StringConstant("yPos") | StringConstant("zPos") => true
      case VariableAccess("xPos", _) | VariableAccess("yPos", _) | VariableAccess("zPos", _) => true
      case _ => false
    }, field.field.boundaryConditions.get).isDefined) {
      field.fieldLayout.discretization match {
        case "node"   => statements += new InitGeomCoords(field.field, true)
        // FIXME: check boundary handling for 26p comm and non-node discr
        case "cell"   => statements += new InitGeomCoords(field.field, true, new MultiIndex((neigh.dir ++ Array(0)).map(i => 0.5 * i)))
        case "face_x" => statements += new InitGeomCoords(field.field, true, new MultiIndex((neigh.dir ++ Array(0)).map(i => if (0 == i) 0.0 else 0.5 * i)))
        case "face_y" => statements += new InitGeomCoords(field.field, true, new MultiIndex((neigh.dir ++ Array(0)).map(i => if (1 == i) 0.0 else 0.5 * i)))
        case "face_z" => statements += new InitGeomCoords(field.field, true, new MultiIndex((neigh.dir ++ Array(0)).map(i => if (2 == i) 0.0 else 0.5 * i)))
      }
    }

    for (vecDim <- 0 until field.field.vectorSize) { // FIXME: this works for now, but users may want to specify bc's per vector element
      var index = LoopOverDimensions.defIt
      index(Knowledge.dimensionality) = vecDim
      var fieldSel = new FieldSelection(field.field, field.level, field.slot, Some(vecDim), field.fragIdx)

      field.fieldLayout.discretization match {
        case d if "node" == d
          || ("face_x" == d && 0 != neigh.dir(0))
          || ("face_y" == d && 0 != neigh.dir(1))
          || ("face_z" == d && 0 != neigh.dir(2)) =>
          if (StringConstant("Neumann") == field.field.boundaryConditions.get)
            Knowledge.experimental_NeumannOrder match {
              case 1 => statements += new AssignmentStatement(new DirectFieldAccess(fieldSel, index), new DirectFieldAccess(fieldSel, index + new MultiIndex((neigh.dir ++ Array(0)).map(i => -i))))
              case 2 => statements += new AssignmentStatement(new DirectFieldAccess(fieldSel, index),
                ((4.0 / 3.0) * new DirectFieldAccess(fieldSel, index + new MultiIndex((neigh.dir ++ Array(0)).map(i => -i))))
                  + ((-1.0 / 3.0) * new DirectFieldAccess(fieldSel, index + new MultiIndex((neigh.dir ++ Array(0)).map(i => -2 * i)))))
              case 3 => // TODO: do we want this? what do we do on the coarser levels? 
                statements += new AssignmentStatement(new DirectFieldAccess(fieldSel, index),
                  (((3.0 * 6.0 / 11.0) * new DirectFieldAccess(fieldSel, index + new MultiIndex((neigh.dir ++ Array(0)).map(i => -1 * i))))
                    + ((-3.0 / 2.0 * 6.0 / 11.0) * new DirectFieldAccess(fieldSel, index + new MultiIndex((neigh.dir ++ Array(0)).map(i => -2 * i))))
                    + ((1.0 / 3.0 * 6.0 / 11.0) * new DirectFieldAccess(fieldSel, index + new MultiIndex((neigh.dir ++ Array(0)).map(i => -3 * i))))))
            }
          else
            statements += new AssignmentStatement(new DirectFieldAccess(fieldSel, index), Duplicate(field.field.boundaryConditions.get))
        case d if "cell" == d
          || ("face_x" == d && 0 == neigh.dir(0))
          || ("face_y" == d && 0 == neigh.dir(1))
          || ("face_z" == d && 0 == neigh.dir(2)) =>
          if (StringConstant("Neumann") == field.field.boundaryConditions.get)
            Knowledge.experimental_NeumannOrder match {
              case 1 => statements += new AssignmentStatement(new DirectFieldAccess(fieldSel, index + new MultiIndex((neigh.dir ++ Array(0)))),
                new DirectFieldAccess(fieldSel, index))
            }
          else
            statements += new AssignmentStatement(new DirectFieldAccess(fieldSel, index + new MultiIndex((neigh.dir ++ Array(0)))),
              (2.0 * Duplicate(field.field.boundaryConditions.get)) - new DirectFieldAccess(fieldSel, index))
      }
    }

    statements
  }

  override def expand : Output[Statement] = {
    if (field.field.boundaryConditions.isDefined) {
      new LoopOverFragments(
        new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
          neighbors.map({ neigh =>
            val loopOverDims = new LoopOverDimensions(Knowledge.dimensionality, neigh._2, setupFieldUpdate(neigh._1)) with OMP_PotentiallyParallel with PolyhedronAccessable
            loopOverDims.optLevel = 1
            new ConditionStatement(UnaryExpression(UnaryOperators.Not, iv.NeighborIsValid(field.domainIndex, neigh._1.index)), loopOverDims) : Statement
          }))) with OMP_PotentiallyParallel
    } else {
      NullStatement
    }
  }
}
