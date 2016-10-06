package exastencils.boundary.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.knowledge.NeighborInfo
import exastencils.multiGrid.HandleBoundaries
import exastencils.prettyprinting.PpStream

/// IR_ApplyBCFunction

case class IR_ApplyBCFunction(
    var name : String,
    var fieldSelection : IR_FieldSelection,
    var neighbors : ListBuffer[NeighborInfo],
    var insideFragLoop : Boolean) extends IR_AbstractFunction with IR_Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl() = prettyprint

  def numDimsGrid = fieldSelection.field.fieldLayout.numDimsGrid

  def resolveIndex(indexId : String, dim : Int) = fieldSelection.field.fieldLayout.idxById(indexId, dim)

  def genIndicesBoundaryHandling(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)] = {

    curNeighbors.map(neigh => (neigh, IR_ExpressionIndexRange(
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map(dim =>
          fieldSelection.fieldLayout.discretization match {
            case discretization if "node" == discretization
              || ("face_x" == discretization && 0 == dim)
              || ("face_y" == discretization && 1 == dim)
              || ("face_z" == discretization && 2 == dim) => dim match {
              case i if neigh.dir(i) == 0 => resolveIndex("GLB", i) // DLB, GLB
              case i if neigh.dir(i) < 0  => resolveIndex("DLB", i) // DLB, GLB
              case i if neigh.dir(i) > 0  => resolveIndex("DRB", i)
            }
            case discretization if "cell" == discretization
              || ("face_x" == discretization && 0 != dim)
              || ("face_y" == discretization && 1 != dim)
              || ("face_z" == discretization && 2 != dim) => dim match {
              case i if neigh.dir(i) == 0 => resolveIndex("GLB", i) // DLB, GLB
              case i if neigh.dir(i) < 0  => resolveIndex("DLB", i)
              case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) - 1
            }
          })),
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map(dim =>
          fieldSelection.fieldLayout.discretization match {
            case discretization if "node" == discretization
              || ("face_x" == discretization && 0 == dim)
              || ("face_y" == discretization && 1 == dim)
              || ("face_z" == discretization && 2 == dim) => dim match {
              case i if neigh.dir(i) == 0 => resolveIndex("GRE", i) // DRE, GRE
              case i if neigh.dir(i) < 0  => resolveIndex("DLE", i)
              case i if neigh.dir(i) > 0  => resolveIndex("DRE", i) // DRE, GRE
            }
            case discretization if "cell" == discretization
              || ("face_x" == discretization && 0 != dim)
              || ("face_y" == discretization && 1 != dim)
              || ("face_z" == discretization && 2 != dim) => dim match {
              case i if neigh.dir(i) == 0 => resolveIndex("GRE", i) // DRE, GRE
              case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) + 1
              case i if neigh.dir(i) > 0  => resolveIndex("DRE", i)
            }
          })))))
  }

  def compileBody(fieldSelection : IR_FieldSelection) : ListBuffer[IR_Statement] = {
    var body = ListBuffer[IR_Statement]()

    val boundaryNeighs = neighbors.filter(neigh => 1 == neigh.dir.count(_ != 0)) // exactly one non-zero entry
    body += HandleBoundaries(fieldSelection, genIndicesBoundaryHandling(boundaryNeighs))

    body
  }

  override def expand() : Output[IR_Function] = {
    // compile function arguments
    var fctArgs = ListBuffer[IR_FunctionArgument]()
    fctArgs += IR_FunctionArgument("slot", IR_IntegerDatatype)
    if (insideFragLoop)
      fctArgs += IR_FunctionArgument(IR_LoopOverFragments.defIt, IR_IntegerDatatype)

    // emit compiled function
    IR_Function(IR_UnitDatatype, name, fctArgs, compileBody(fieldSelection))
  }
}
