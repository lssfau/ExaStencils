package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expandable
import exastencils.base.ir.IR_ExpressionStatement
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_MemberFunctionCallArrow
import exastencils.base.ir.IR_Neq
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.datastructures.Transformation.OutputType
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger
import exastencils.waLBerla.ir.IR_WaLBerlaUtil.getBlocks

case class IR_WaLBerlaSwapFieldPointers(
    var srcAcc : IR_FieldAccess,
    var dstAcc : IR_FieldAccess
) extends IR_Statement with IR_Expandable {

  override def expand() : OutputType = {
    if ( !(IR_WaLBerlaFieldCollection.contains(srcAcc) && IR_WaLBerlaFieldCollection.contains(dstAcc)) )
      Logger.error("\"IR_WaLBerlaSwapFieldPointers\": Both fields must be waLBerla fields")

    def defIt = IR_VariableAccess("block", IR_SpecialDatatype("auto"))

    val expr = new IR_MemberFunctionCallArrow(WB_IV_FieldData(srcAcc), "swapDataPointers", ListBuffer(WB_IV_FieldData(dstAcc)))
    new IR_ForLoop(
      IR_VariableDeclaration(defIt, IR_MemberFunctionCallArrow(getBlocks, "begin", defIt.datatype)),
      IR_Neq(defIt, IR_MemberFunctionCallArrow(getBlocks, "end", defIt.datatype)),
      IR_ExpressionStatement(IR_PreIncrement(defIt)),
      IR_WaLBerlaUtil.getFields(ListBuffer(srcAcc, dstAcc)) :+ IR_ExpressionStatement(expr))
  }
}
