package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print

/// IR_PrintFieldsAsciiSWE
// provides functions to write geometry and field in ascii format
// encapsulated in trait to minimize duplicate code in ascii writers

trait IR_PrintFieldsAsciiSWE extends IR_PrintVisualizationSWE {

  def printField(name : String, stream : IR_VariableAccess, loopBody : ListBuffer[IR_Statement]) : ListBuffer[IR_Statement] = ListBuffer(
    IR_Print(stream, "std::scientific"),
    IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(etaDiscLower0.domain.index),
        loopBody
      ),
    IR_Print(stream, IR_Print.flush))
  )

  def addNodePrint(name : String, cellPrint : ListBuffer[IR_Expression], optStream : Option[IR_VariableAccess] = None) : ListBuffer[IR_Statement] = {
    val stream = optStream getOrElse newStream

    val print = IR_Print(stream, cellPrint)

    val loopBody = IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
      IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
      IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => nodalLoopEnd + etaDiscLower0.layout.idxById("IE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression))),
      print)

    printField(name, stream, ListBuffer(loopBody))
  }

  def addReducedNodePrint(name : String, discFields : ListBuffer[IR_Field], optStream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = {
    val stream = optStream getOrElse newStream

    printField(name, stream, reducedCellPrint(stream, discFields, indentation))
  }

  def printNodalField(nodeField : IR_Field, stream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = {
    addNodePrint(nodeField.name, {
      var nodePrint = ListBuffer[IR_Expression]()
      nodeOffsets.foreach { offset =>
        if(indentation.isDefined)
          nodePrint += indentation.get
        nodePrint += IR_FieldAccess(nodeField, IR_IV_ActiveSlot(nodeField), IR_LoopOverDimensions.defIt(numDimsGrid) + offset)
        nodePrint += IR_Print.newline
      }
      nodePrint
    }, stream)
  }

  def printDiscField(discField : ListBuffer[IR_Field], stream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = {
    if (discField.length != 6) {
      Logger.error("Wrong usage for \"printDiscField\" in \"IR_PrintFieldsAsciiSWE\". Parameter \"discField\" should have length: 6.")
    }

    if(Knowledge.swe_nodalReductionPrint) {
      addReducedNodePrint(getBasenameDiscField(discField), discField, stream, indentation)
    } else {
      addNodePrint(getBasenameDiscField(discField), {
        var nodePrint = ListBuffer[IR_Expression]()
        discField.foreach { field =>
          if(indentation.isDefined)
            nodePrint += indentation.get
          nodePrint += IR_FieldAccess(field, IR_IV_ActiveSlot(field), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.newline
        }
        nodePrint
      }, stream)
    }
  }
}
