package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_LeveledFunction
import exastencils.base.ir.IR_MemberFunctionCallArrow
import exastencils.base.ir.IR_SharedPointerDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_BlockDataID
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_FieldDatatype
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_IBlock
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_StructuredBlockStorage

object IR_WaLBerlaUtil extends DefaultStrategy("Get waLBerla sweep") {
  def isWaLBerlaKernel(func : IR_Function) : Boolean = func.name.startsWith("walberla_")
  var startNode : Option[IR_LeveledFunction] = None

  val iblock = IR_VariableAccess("block", WB_IBlock)
  val iblockPtr = IR_FunctionArgument(iblock.name, IR_SharedPointerDatatype(WB_IBlock))

  val blockStorage = IR_VariableAccess("blocks", WB_StructuredBlockStorage)
  val blockStoragePtr = IR_VariableAccess(blockStorage.name, IR_SharedPointerDatatype(WB_StructuredBlockStorage))

  def memberSuffix = "_gen"
  def getMemberName(s : String) = s + memberSuffix

  def getBlockDataID(name : String) = IR_VariableAccess(getMemberName(name + "_ID"), WB_BlockDataID)
  def getBlocks = IR_VariableAccess(getMemberName(blockStoragePtr.name), blockStoragePtr.datatype)

  // get field data from block
  def getFields(accesses : ListBuffer[IR_FieldAccess]) = accesses.map(fAcc => {
    val wbField = IR_WaLBerlaField(fAcc.field)
    val fieldDt = WB_FieldDatatype(wbField)
    WB_IV_FieldData(wbField, fAcc.slot, fAcc.fragIdx).getData(
      Some(new IR_MemberFunctionCallArrow(iblock, s"getData< ${fieldDt.typeName} >", ListBuffer(getBlockDataID(fAcc.name)), fieldDt)))
  })

  this += Transformation("Get sweep node", {
    case func : IR_LeveledFunction if isWaLBerlaKernel(func) =>
      if (startNode.isEmpty)
        startNode = Some(func)
      else
        Logger.error("Multiple waLBerla sweep candidates found.")

      func
  })
}
