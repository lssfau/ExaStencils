package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstReferenceDatatype
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_VariableAccess
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_BlockDataID
import exastencils.waLBerla.ir.IR_WaLBerlaUtil._

// store context

case class IR_WaLBerlaInterfaceGenerationContext(var functions : ListBuffer[IR_WaLBerlaFunction]) {

  IR_CollectWaLBerlaFieldAccesses.applyStandalone(functions)
  var fieldNames : ListBuffer[String] = IR_CollectWaLBerlaFieldAccesses.wbFieldAccesses.map(_.name)

  private def toBlockDataID(name : String) = IR_VariableAccess(name + "_ID", WB_BlockDataID)

  def blockDataIDs : Map[String, IR_FunctionArgument] = fieldNames.sorted.map(acc => acc -> IR_FunctionArgument(toBlockDataID(acc))).toMap

  var lowerLevelFieldAccs : ListBuffer[IR_WaLBerlaFieldAccess] = IR_CollectWaLBerlaFieldAccesses.wbFieldAccesses.filter(f => f.level != f.field.maxLevel)

  // ctor params and members
  var ctorParams : ListBuffer[IR_FunctionArgument] = ListBuffer()
  var members : ListBuffer[IR_VariableAccess] = ListBuffer()

  // block data IDs and params of waLBerla function
  ctorParams ++= blockDataIDs.values
  members ++= blockDataIDs.values.map(arg => IR_VariableAccess(getGeneratedName(arg.name), arg.datatype))

  // block storage shared_ptr
  ctorParams += IR_FunctionArgument(IR_VariableAccess(blockStoragePtr.name, IR_ConstReferenceDatatype(blockStoragePtr.datatype)))
  members += IR_VariableAccess(getGeneratedName(blockStoragePtr.name), blockStoragePtr.datatype)

  // pointers to lower-level fields
}
