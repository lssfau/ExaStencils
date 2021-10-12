package exastencils.waLBerla.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_MemberFunctionCallArrow
import exastencils.base.ir.IR_SharedPointerDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.logger.Logger
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_BlockDataID
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_CommScheme
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_FieldDatatype
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_IBlock
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_StructuredBlockForest

object IR_WaLBerlaUtil {

  val iblock = IR_VariableAccess("block", WB_IBlock)
  val iblockPtr = IR_FunctionArgument(iblock.name, IR_SharedPointerDatatype(WB_IBlock))

  val blockForest = IR_VariableAccess("blocks", WB_StructuredBlockForest)
  val blockForestPtr = IR_VariableAccess(blockForest.name, IR_SharedPointerDatatype(WB_StructuredBlockForest))
  val blockForestMember = IR_VariableAccess(getGeneratedName(blockForestPtr.name), blockForestPtr.datatype)

  def make_shared(templateDt : String, arg : IR_Expression) = IR_FunctionCall(IR_ExternalFunctionReference(s"make_shared< $templateDt >"), arg)

  // TODO: this is only a temporary implementation (Q potentially too large, i.e. too many directions)
  def stencilTemplate(numDims : Int) = numDims match {
    case 2 => "stencil::D2Q9"
    case 3 => "stencil::D2Q27"
    case _ => Logger.error("No waLBerla stencil class available for dimension: " + numDims)
  }
  def commScheme(wbField : IR_WaLBerlaField) = IR_VariableAccess(getGeneratedName(s"commScheme_${wbField.codeName}"), WB_CommScheme(stencilTemplate(wbField.numDimsGrid)))
  def createUniformPackInfo(wbField : IR_WaLBerlaField, fieldBlockDataID : IR_Access) =
    make_shared(s"field::communication::PackInfo< ${WB_FieldDatatype(wbField).prettyprint()} >", fieldBlockDataID)

  def memberSuffix = "_gen"
  def getGeneratedName(s : String) : String = s + memberSuffix

  def getBlockDataID(name : String) = IR_VariableAccess(getGeneratedName(name + "_ID"), WB_BlockDataID)
  def getBlocks = IR_VariableAccess(getGeneratedName(blockForestPtr.name), blockForestPtr.datatype)

  // get field data from block
  def getFields(accesses : IR_WaLBerlaFieldAccess*) : ListBuffer[IR_VariableDeclaration] = accesses.to[mutable.ListBuffer].map(fAcc => {
    val fieldDt = WB_FieldDatatype(fAcc.target)
    IR_IV_WaLBerlaFieldData(fAcc).getData(
      Some(new IR_MemberFunctionCallArrow(iblock, s"getData< ${fieldDt.typeName} >", ListBuffer(getBlockDataID(fAcc.name)), fieldDt)))
  })

  def getFields(accesses : IR_WaLBerlaField*)(implicit d : DummyImplicit) : ListBuffer[IR_VariableDeclaration] = accesses.to[mutable.ListBuffer].map(field => {
    val fieldDt = WB_FieldDatatype(field)
    IR_IV_WaLBerlaFieldData(field).getData(
      Some(new IR_MemberFunctionCallArrow(iblock, s"getData< ${fieldDt.typeName} >", ListBuffer(getBlockDataID(field.name)), fieldDt)))
  })
}
