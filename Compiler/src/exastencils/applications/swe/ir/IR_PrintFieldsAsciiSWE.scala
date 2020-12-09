package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_StringConstant
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_IV_ActiveSlot
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

  def printBath(stream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = {
    addNodePrint("bath", {
      var nodePrint = ListBuffer[IR_Expression]()
      nodeOffsets.foreach { offset =>
        if(indentation.isDefined)
          nodePrint += indentation.get
        nodePrint += IR_FieldAccess(bath, IR_IV_ActiveSlot(bath), IR_LoopOverDimensions.defIt(numDimsGrid) + offset)
        nodePrint += IR_Print.newline
      }
      nodePrint
    }, stream)
  }

  def printEta(stream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = {
    if(Knowledge.swe_nodalReductionPrint) {
      addReducedNodePrint("eta", etaDisc, stream, indentation)
    } else {
      addNodePrint("eta", {
        var nodePrint = ListBuffer[IR_Expression]()
        etaDisc.foreach { eta =>
          if(indentation.isDefined)
            nodePrint += indentation.get
          nodePrint += IR_FieldAccess(eta, IR_IV_ActiveSlot(eta), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.newline
        }
        nodePrint
      }, stream)
    }
  }

  def printU(stream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = {
    if(Knowledge.swe_nodalReductionPrint) {
      addReducedNodePrint("u", uDisc, stream, indentation)
    } else {
      addNodePrint("u", {
        var nodePrint = ListBuffer[IR_Expression]()
        uDisc.foreach { u =>
          if(indentation.isDefined)
            nodePrint += indentation.get
          nodePrint += IR_FieldAccess(u, IR_IV_ActiveSlot(u), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.newline
        }
        nodePrint
      }, stream)
    }
  }

  def printV(stream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = {
    if(Knowledge.swe_nodalReductionPrint) {
      addReducedNodePrint("v", vDisc, stream, indentation)
    } else {
      addNodePrint("v", {
        var nodePrint = ListBuffer[IR_Expression]()
        vDisc.foreach { v =>
          if(indentation.isDefined)
            nodePrint += indentation.get
          nodePrint += IR_FieldAccess(v, IR_IV_ActiveSlot(v), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.newline
        }
        nodePrint
      }, stream)
    }
  }

  def printOrder(stream : Option[IR_VariableAccess] = None, indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = {
    if (optLocalOrderLower.isDefined && optLocalOrderUpper.isDefined) {
      if(Knowledge.swe_nodalReductionPrint) {
        addReducedNodePrint("order", ListBuffer() ++ (0 until 3).map(_ => optLocalOrderLower.get) ++ (0 until 3).map(_ => optLocalOrderUpper.get), stream, indentation)
      } else {
        addNodePrint("order", {
          var nodePrint = ListBuffer[IR_Expression]()
          List(optLocalOrderLower.get, optLocalOrderUpper.get).foreach { f =>
            for (_ <- 0 until 3) { // TODO: cell data instead of
              if(indentation.isDefined)
                nodePrint += indentation.get
              nodePrint += IR_FieldAccess(f, IR_IV_ActiveSlot(f), IR_LoopOverDimensions.defIt(numDimsGrid))
              nodePrint += IR_Print.newline
            }
          }
          nodePrint
        }, stream)
      }
    } else {
      ListBuffer[IR_Statement]()
    }
  }
}
