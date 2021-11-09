package exastencils.waLBerla.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_InitializerList
import exastencils.base.ir.IR_MemberFunctionCallArrow
import exastencils.base.ir.IR_SharedPointerDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.logger.Logger
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_BlockDataID
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_FieldDatatype
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_IBlock
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_StructuredBlockForest

object IR_WaLBerlaUtil {

  private val iblock = IR_VariableAccess("block", WB_IBlock)
  private val iblockPtr = IR_FunctionArgument(iblock.name, IR_SharedPointerDatatype(WB_IBlock))

  val blockForest = IR_VariableAccess("blocks", WB_StructuredBlockForest)
  val blockForestPtr = IR_VariableAccess(blockForest.name, IR_SharedPointerDatatype(WB_StructuredBlockForest))
  val blockForestMember = IR_VariableAccess(getGeneratedName(blockForestPtr.name), blockForestPtr.datatype)

  def make_shared(templateDt : String, arg : IR_Expression) = IR_FunctionCall(IR_ExternalFunctionReference(s"make_shared< $templateDt >"), arg)

  // TODO: this is only a temporary implementation (Q potentially too large, i.e. too many directions)
  def stencilTemplate(numDims : Int) = numDims match {
    case 2 => "stencil::D2Q9"
    case 3 => "stencil::D3Q27"
    case _ => Logger.error("No waLBerla stencil class available for dimension: " + numDims)
  }
  def createUniformPackInfo(wbField : IR_WaLBerlaField, fieldBlockDataID : IR_Access) =
    make_shared(s"field::communication::PackInfo< ${WB_FieldDatatype(wbField).prettyprint()} >", fieldBlockDataID)

  def memberSuffix = "_gen"
  def getGeneratedName(s : String) : String = s + memberSuffix

  def createBlockDataIdName(wbField : IR_WaLBerlaField, slot : Int) = wbField.name + "_ID" + (if (wbField.numSlots > 1) s"_$slot" else "")
  def getBlockDataID(wbField : IR_WaLBerlaField, slot : Int) =
    IR_VariableAccess(getGeneratedName(createBlockDataIdName(wbField, slot)), WB_BlockDataID)
  def getBlockForest = blockForestMember

  // get field data from block
  def getDataFromBlock(wbField : IR_WaLBerlaField, slot : Int) = {
    val fieldDt = WB_FieldDatatype(wbField)
    new IR_MemberFunctionCallArrow(iblock, s"getData< ${fieldDt.typeName} >", ListBuffer(getBlockDataID(wbField, slot)), fieldDt)
  }

  def getFields(accesses : IR_WaLBerlaFieldAccess*) : ListBuffer[IR_VariableDeclaration] = {
    accesses.to[mutable.ListBuffer].map(fAcc => {
      val wbField = fAcc.field
      val defValue = if (wbField.numSlots > 1) {
        IR_InitializerList( (0 until wbField.numSlots).map(slot => getDataFromBlock(wbField, slot)) : _* )
      } else {
        getDataFromBlock(wbField, 0)
      }
      IR_IV_WaLBerlaFieldData(fAcc).getData(Some(defValue))
    })
  }
}
