package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer

import exastencils.applications.ns.ir.IR_PrintVtkNNF
import exastencils.applications.ns.ir.IR_PrintVtkNS
import exastencils.applications.swe.ir.IR_PrintVtkSWE
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionStatement
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_UnresolvedFunctionReference
import exastencils.base.ir.IR_VariableAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldIO
import exastencils.logger.Logger

trait IR_PrintVisualization {
  def filename : IR_Expression
  def numDimsGrid : Int

  def newStream = IR_VariableAccess(IR_FieldIO.getNewStreamName(), IR_SpecialDatatype("std::ofstream"))

  def level : Int

  def numPointsPerFrag : Int
  def numFrags : IR_Expression

  def numCells_x : Int
  def numCells_y : Int
  def numCells_z : Int

  def numNodes = numPointsPerFrag * numFrags

  def someCellField : IR_Field // required as base for setting up iteration spaces later
}

/// IR_ResolveVisualizationPrinters

object IR_ResolveVisualizationPrinters extends DefaultStrategy("IR_ResolveVisualizationPrinters") {
  this += new Transformation("ResolveFunctionCalls", {
    // vtk printers
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printVtkSWE", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i)) => IR_PrintVtkSWE(s, i.toInt)
        case _                                                    => Logger.error("Malformed call to printVtkSWE; usage: printVtkSWE ( \"filename\", level )")
      }

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printVtkNS", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i)) => IR_PrintVtkNS(s, i.toInt)
        case _                                                    => Logger.error("Malformed call to printVtkNS; usage: printVtkNS ( \"filename\", level )")
      }

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printVtkNNF", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i)) => IR_PrintVtkNNF(s, i.toInt)
        case _                                                    => Logger.error("Malformed call to printVtkNNF; usage: printVtkNNF ( \"filename\", level )")
      }
    // TODO: resolve calls to xdmf printer
    // TODO: resolve calls to exodus printer
  })
}
