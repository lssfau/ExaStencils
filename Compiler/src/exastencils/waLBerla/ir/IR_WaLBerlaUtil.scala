package exastencils.waLBerla.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_MemberFunctionCallArrow
import exastencils.base.ir.IR_SharedPointerDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_BlockDataID
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_FieldDatatype
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_IBlock
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_StructuredBlockStorage

object IR_WaLBerlaUtil {

  val iblock = IR_VariableAccess("block", WB_IBlock)
  val iblockPtr = IR_FunctionArgument(iblock.name, IR_SharedPointerDatatype(WB_IBlock))

  val blockStorage = IR_VariableAccess("blocks", WB_StructuredBlockStorage)
  val blockStoragePtr = IR_VariableAccess(blockStorage.name, IR_SharedPointerDatatype(WB_StructuredBlockStorage))
  val blockStorageMember = IR_VariableAccess(getGeneratedName(blockStoragePtr.name), blockStoragePtr.datatype)

  def memberSuffix = "_gen"
  def getGeneratedName(s : String) : String = s + memberSuffix

  def getBlockDataID(name : String) = IR_VariableAccess(getGeneratedName(name + "_ID"), WB_BlockDataID)
  def getBlocks = IR_VariableAccess(getGeneratedName(blockStoragePtr.name), blockStoragePtr.datatype)

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
