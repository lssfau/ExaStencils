package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstReferenceDatatype
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.core.Duplicate
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_BlockDataID
import exastencils.waLBerla.ir.IR_WaLBerlaUtil._

// store context

object IR_WaLBerlaFunctionGenerationContext {
  def apply(func : IR_WaLBerlaFunction) : IR_WaLBerlaFunctionGenerationContext = new IR_WaLBerlaFunctionGenerationContext(func.parameters, func.body)
}

case class IR_WaLBerlaFunctionGenerationContext(
    private var parameterList : ListBuffer[IR_FunctionArgument],
    private var body : ListBuffer[IR_Statement]
) {

  IR_CollectWaLBerlaFieldAccesses.applyStandalone(body)
  var fieldNames : ListBuffer[String] = IR_CollectWaLBerlaFieldAccesses.wbFieldAccesses.map(_.name).sorted

  private def toBlockDataID(name : String) = IR_VariableAccess(name + "_ID", WB_BlockDataID)

  def blockDataIDs : Map[String, IR_FunctionArgument] = fieldNames.map(acc => acc -> IR_FunctionArgument(toBlockDataID(acc))).toMap

  // modified function params
  var newFunctionParams : ListBuffer[IR_FunctionArgument] = Duplicate(parameterList)

  // block data IDs and params of waLBerla function
  newFunctionParams ++= blockDataIDs.values

  // block storage shared_ptr
  newFunctionParams += IR_FunctionArgument(IR_VariableAccess(blockStoragePtr.name, IR_ConstReferenceDatatype(blockStoragePtr.datatype)))
}
