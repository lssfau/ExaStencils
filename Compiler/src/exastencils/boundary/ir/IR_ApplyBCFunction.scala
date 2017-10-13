package exastencils.boundary.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.NeighborInfo
import exastencils.core.Duplicate
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.grid.ir._

/// IR_ApplyBCFunction

case class IR_ApplyBCFunction(
    var name : String,
    var level : Int,
    var fieldSelection : IR_FieldSelection,
    var neighbors : ListBuffer[NeighborInfo],
    var insideFragLoop : Boolean) extends IR_FutureLeveledFunction {

  override def prettyprint_decl() = prettyprint

  def numDimsGrid = fieldSelection.field.fieldLayout.numDimsGrid

  def resolveIndex(indexId : String, dim : Int) = fieldSelection.field.fieldLayout.idxById(indexId, dim)

  def genIndicesBoundaryHandling(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)] = {

    curNeighbors.map(neigh => (neigh, IR_ExpressionIndexRange(
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map(dim =>
          fieldSelection.fieldLayout.localization match {
            case IR_AtNode | IR_AtFaceCenter(`dim`)   => dim match {
              case i if neigh.dir(i) == 0 => resolveIndex("GLB", i) // DLB, GLB
              case i if neigh.dir(i) < 0  => resolveIndex("DLB", i) // DLB, GLB
              case i if neigh.dir(i) > 0  => resolveIndex("DRB", i)
            }
            case IR_AtCellCenter | IR_AtFaceCenter(_) => dim match {
              case i if neigh.dir(i) == 0 => resolveIndex("GLB", i) // DLB, GLB
              case i if neigh.dir(i) < 0  => resolveIndex("DLB", i)
              case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) - 1
            }
          })),
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map(dim =>
          fieldSelection.fieldLayout.localization match {
            case IR_AtNode | IR_AtFaceCenter(`dim`)   => dim match {
              case i if neigh.dir(i) == 0 => resolveIndex("GRE", i) // DRE, GRE
              case i if neigh.dir(i) < 0  => resolveIndex("DLE", i)
              case i if neigh.dir(i) > 0  => resolveIndex("DRE", i) // DRE, GRE
            }
            case IR_AtCellCenter | IR_AtFaceCenter(_) => dim match {
              case i if neigh.dir(i) == 0 => resolveIndex("GRE", i) // DRE, GRE
              case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) + 1
              case i if neigh.dir(i) > 0  => resolveIndex("DRE", i)
            }
          })))))
  }

  def compileBody(fieldSelection : IR_FieldSelection) : ListBuffer[IR_Statement] = {
    var body = ListBuffer[IR_Statement]()

    val boundaryNeighs = neighbors.filter(neigh => 1 == neigh.dir.count(_ != 0)) // exactly one non-zero entry
    body += IR_HandleBoundaries(Duplicate(fieldSelection), genIndicesBoundaryHandling(boundaryNeighs))

    body
  }

  override def generateFct() = {
    // compile function arguments
    var fctArgs = ListBuffer[IR_FunctionArgument]()
    fctArgs += IR_FunctionArgument("slot", IR_IntegerDatatype)
    if (insideFragLoop)
      fctArgs += IR_FunctionArgument(IR_LoopOverFragments.defIt)

    // emit compiled function
    IR_LeveledFunction(name, level, IR_UnitDatatype, fctArgs, compileBody(fieldSelection))
  }
}
