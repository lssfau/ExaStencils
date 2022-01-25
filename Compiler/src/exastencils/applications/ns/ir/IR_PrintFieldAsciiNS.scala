package exastencils.applications.ns.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.util.ir.IR_Print

/// IR_PrintFieldAsciiNS
// provides functions to write geometry and field data in ascii format
// encapsulated in trait to minimize duplicate code in ascii writers

trait IR_PrintFieldAsciiNS extends IR_PrintVisualizationNS {

  def printField(name : String, stream : IR_VariableAccess, loopBody : ListBuffer[IR_Statement], numComponents : Int = 1) : ListBuffer[IR_Statement] = ListBuffer(
    IR_Print(stream, "std::scientific"),
    IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(p.domain.index),
        loopBody
      ),
      IR_Print(stream, IR_Print.flush))
  )

  def addCellPrint(name : String, cellPrint : ListBuffer[IR_Expression], numComponents : Int = 1, optStream : Option[IR_VariableAccess] = None) : ListBuffer[IR_Statement] = {
    val stream = optStream getOrElse newStream

    val print = IR_Print(stream, cellPrint)

    val loopBody = IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
      IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DLB", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression)),
      IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DRE", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression))),
      print)

    printField(name, stream, ListBuffer(loopBody), numComponents)
  }

  def printVel(stream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = addCellPrint("vel", {
    var cellPrint = ListBuffer[IR_Expression]()
    if(indentation.isDefined)
      cellPrint += indentation.get
    cellPrint += meanU
    cellPrint += IR_StringConstant(" ")
    cellPrint += meanV
    if (numDimsGrid > 2) {
      cellPrint += IR_StringConstant(" ")
      cellPrint += meanW
    }
    cellPrint += IR_Print.newline
  }, numDimsGrid, stream)

  def printP(stream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = addCellPrint("p", {
    var cellPrint = ListBuffer[IR_Expression]()
    if(indentation.isDefined)
      cellPrint += indentation.get
    cellPrint += IR_FieldAccess(p, IR_IV_ActiveSlot(p), IR_LoopOverDimensions.defIt(numDimsGrid))
    cellPrint += IR_Print.newline
  }, optStream = stream)

  def printRho(stream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = addCellPrint("rho", {
    var cellPrint = ListBuffer[IR_Expression]()
    if(indentation.isDefined)
      cellPrint += indentation.get
    cellPrint += IR_FieldAccess(rho, IR_IV_ActiveSlot(rho), IR_LoopOverDimensions.defIt(numDimsGrid))
    cellPrint += IR_Print.newline
  }, optStream = stream)

  def printMue(stream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = addCellPrint("mue", {
    var cellPrint = ListBuffer[IR_Expression]()
    if(indentation.isDefined)
      cellPrint += indentation.get
    cellPrint += IR_FieldAccess(mue, IR_IV_ActiveSlot(mue), IR_LoopOverDimensions.defIt(numDimsGrid))
    cellPrint += IR_Print.newline
  }, optStream = stream)

  def printGamma(stream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = addCellPrint("gamma", {
    var cellPrint = ListBuffer[IR_Expression]()
    if(indentation.isDefined)
      cellPrint += indentation.get
    cellPrint += IR_FieldAccess(gamma, IR_IV_ActiveSlot(gamma), IR_LoopOverDimensions.defIt(numDimsGrid))
    cellPrint += IR_Print.newline
  }, optStream = stream)

  def printPhi(stream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = addCellPrint("phi", {
    var cellPrint = ListBuffer[IR_Expression]()
    if(indentation.isDefined)
      cellPrint += indentation.get
    cellPrint += IR_FieldAccess(phi, IR_IV_ActiveSlot(phi), IR_LoopOverDimensions.defIt(numDimsGrid))
    cellPrint += IR_Print.newline
  }, optStream = stream)
}
