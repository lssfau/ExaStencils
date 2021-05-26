package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstReferenceDatatype
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_LeveledFunction
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_BlockDataID
import exastencils.waLBerla.ir.IR_WaLBerlaUtil._

// store context

case class IR_WaLBerlaSweepGenerationContext(func : IR_LeveledFunction)  {

  var fields : ListBuffer[IR_WaLBerlaField] = IR_WaLBerlaFieldCollection.objects
  val parameters : ListBuffer[IR_FunctionArgument] = func.parameters
  val body : ListBuffer[IR_Statement] = func.body

  private def toBlockDataID(name : String) = IR_VariableAccess(name + "_ID", WB_BlockDataID)

  def blockDataIDs : Map[String, IR_FunctionArgument] = fields.map(acc => acc.name -> IR_FunctionArgument(toBlockDataID(acc.name))).toMap

  // extract ctor params
  var ctorParams : ListBuffer[IR_FunctionArgument] = ListBuffer()

  // block data IDs and params of waLBerla function
  ctorParams ++= blockDataIDs.values
  ctorParams ++= parameters

  // create member for each ctor param
  val members : ListBuffer[IR_VariableAccess] = ctorParams.map(param => IR_VariableAccess(param.name + "_", param.datatype))

  // block storage shared_ptr
  ctorParams += IR_FunctionArgument(IR_VariableAccess(blockStoragePtr.name, IR_ConstReferenceDatatype(blockStoragePtr.datatype)))
  members += IR_VariableAccess(blockStoragePtr.name + "_", blockStoragePtr.datatype)
}
