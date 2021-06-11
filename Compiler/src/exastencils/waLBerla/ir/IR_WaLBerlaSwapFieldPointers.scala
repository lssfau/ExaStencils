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
    srcAcc : IR_FieldAccess,
    dstAcc : IR_FieldAccess
) extends IR_Statement with IR_Expandable {

  private def toWBAcc(wbf : IR_WaLBerlaField, fAcc : IR_FieldAccess) = IR_WaLBerlaFieldAccess(wbf, fAcc.fragIdx, fAcc.index, fAcc.offset, fAcc.frozen, fAcc.matIndex)

  val wbSrc : IR_WaLBerlaFieldAccess = toWBAcc( IR_WaLBerlaFieldCollection.getByIdentifier(srcAcc.name, srcAcc.level, suppressError = true).get, srcAcc )
  val wbDst : IR_WaLBerlaFieldAccess = toWBAcc( IR_WaLBerlaFieldCollection.getByIdentifier(dstAcc.name, dstAcc.level, suppressError = true).get, dstAcc )

  override def expand() : OutputType = {
    if ( !(IR_WaLBerlaFieldCollection.contains(srcAcc) && IR_WaLBerlaFieldCollection.contains(dstAcc)) )
      Logger.error("\"IR_WaLBerlaSwapFieldPointers\": Both fields must be waLBerla fields")

    def defIt = IR_VariableAccess("block", IR_SpecialDatatype("auto"))

    val expr = new IR_MemberFunctionCallArrow(IR_IV_WaLBerlaFieldData(srcAcc), "swapDataPointers", ListBuffer(IR_IV_WaLBerlaFieldData(dstAcc)))
    new IR_ForLoop(
      IR_VariableDeclaration(defIt, IR_MemberFunctionCallArrow(getBlocks, "begin", defIt.datatype)),
      IR_Neq(defIt, IR_MemberFunctionCallArrow(getBlocks, "end", defIt.datatype)),
      IR_ExpressionStatement(IR_PreIncrement(defIt)),
      IR_WaLBerlaUtil.getFields(wbSrc, wbDst) :+ IR_ExpressionStatement(expr))
  }
}
