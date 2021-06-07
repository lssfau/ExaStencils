package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstReferenceDatatype
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_BlockDataID
import exastencils.waLBerla.ir.IR_WaLBerlaUtil._

// store context

object IR_WaLBerlaFunctorGenerationContext {
  def apply(functor : IR_WaLBerlaFunctor) : IR_WaLBerlaFunctorGenerationContext = new IR_WaLBerlaFunctorGenerationContext(functor.name, functor.parameters, functor.body)
}


case class IR_WaLBerlaFunctorGenerationContext(
    var name : String,
    var parameters : ListBuffer[IR_FunctionArgument],
    var body : ListBuffer[IR_Statement]
) {

  IR_CollectWaLBerlaFieldAccesses.applyStandalone(body)
  var fieldNames : ListBuffer[String] = IR_CollectWaLBerlaFieldAccesses.wbFieldAccesses.map(_.name) // IR_WaLBerlaUtil.functorAccessedFields.getOrElse(name, ListBuffer()).sorted

  def className : String = name.replaceFirst("walberla_", "")

  private def toBlockDataID(name : String) = IR_VariableAccess(name + "_ID", WB_BlockDataID)

  def blockDataIDs : Map[String, IR_FunctionArgument] = fieldNames.map(acc => acc -> IR_FunctionArgument(toBlockDataID(acc))).toMap

  // extract ctor params
  var ctorParams : ListBuffer[IR_FunctionArgument] = ListBuffer()

  // block data IDs and params of waLBerla function
  ctorParams ++= parameters
  ctorParams ++= blockDataIDs.values

  // create member for each ctor param
  val members : ListBuffer[IR_VariableAccess] = ctorParams.map(param => IR_VariableAccess(getMemberName(param.name), param.datatype))

  // block storage shared_ptr
  ctorParams += IR_FunctionArgument(IR_VariableAccess(blockStoragePtr.name, IR_ConstReferenceDatatype(blockStoragePtr.datatype)))
  members += IR_VariableAccess(getMemberName(blockStoragePtr.name), blockStoragePtr.datatype)
}
