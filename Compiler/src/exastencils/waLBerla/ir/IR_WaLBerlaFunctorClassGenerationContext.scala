package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstReferenceDatatype
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_BlockDataID
import exastencils.waLBerla.ir.IR_WaLBerlaUtil._

// store context

object IR_WaLBerlaFunctorClassGenerationContext {
  def apply(functorClass : IR_WaLBerlaFunctorClass) : IR_WaLBerlaFunctorClassGenerationContext =
    new IR_WaLBerlaFunctorClassGenerationContext(functorClass.functors.map(_.parameters), functorClass.functors.map(_.body))
}

case class IR_WaLBerlaFunctorClassGenerationContext(
    var parameterLists : ListBuffer[ListBuffer[IR_FunctionArgument]],
    var bodies : ListBuffer[ListBuffer[IR_Statement]]
) {

  IR_CollectWaLBerlaFieldAccesses.applyStandalone(bodies)
  var fieldNames : ListBuffer[String] = IR_CollectWaLBerlaFieldAccesses.wbFieldAccesses.map(_.name).sorted

  private def toBlockDataID(name : String) = IR_VariableAccess(name + "_ID", WB_BlockDataID)

  def blockDataIDs : Map[String, IR_FunctionArgument] = fieldNames.map(acc => acc -> IR_FunctionArgument(toBlockDataID(acc))).toMap

  // extract ctor params
  var ctorParams : ListBuffer[IR_FunctionArgument] = ListBuffer()

  // block data IDs and params of waLBerla function
  ctorParams ++= parameterLists.flatten
  ctorParams ++= blockDataIDs.values

  // create member for each ctor param
  val members : ListBuffer[IR_VariableAccess] = ctorParams.map(param => IR_VariableAccess(getMemberName(param.name), param.datatype))

  // block storage shared_ptr
  ctorParams += IR_FunctionArgument(IR_VariableAccess(blockStoragePtr.name, IR_ConstReferenceDatatype(blockStoragePtr.datatype)))
  members += IR_WaLBerlaUtil.blockStorageMember
}
