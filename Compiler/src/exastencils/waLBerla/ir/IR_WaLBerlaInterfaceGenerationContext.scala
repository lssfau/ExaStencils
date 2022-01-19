package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._

// store context

case class IR_WaLBerlaInterfaceGenerationContext(var functions : ListBuffer[IR_WaLBerlaFunction]) {

  val uniqueWbFields = IR_WaLBerlaFieldCollection.objects.groupBy(_.name).map(_._2.head).to[ListBuffer] // find unique wb fields
    .sortBy(_.name)

  // ctor params and members
  var ctorParams : ListBuffer[IR_FunctionArgument] = ListBuffer()
  var members : ListBuffer[IR_VariableAccess] = ListBuffer()
  var ctorInitializerList : IR_MemberInitializerList = IR_MemberInitializerList()

  // block data IDs and params of waLBerla function
  val blockDataIDs = uniqueWbFields.map(wbf => IR_WaLBerlaBlockDataID(wbf, slot = 0))
  ctorParams ++= blockDataIDs.map(_.ctorParameter)
  members ++= blockDataIDs.map(_.member)
  for (dataId <- blockDataIDs) ctorInitializerList.addEntry(dataId.initializerListEntry)

  // block storage shared_ptr
  val blockForest = IR_WaLBerlaBlockForest()
  ctorParams += blockForest.ctorParameter
  members += blockForest.member
  ctorInitializerList.addEntry(blockForest.initializerListEntry)

  // comm scheme for each field. packed with a uniform pack info
  if (IR_WaLBerlaUtil.initCommSchemes) {
    for (wbf <- uniqueWbFields) {
      val commScheme = IR_WaLBerlaCommScheme(wbf, slot = 0)
      members += commScheme.baseAccess()
    }
  }

  // ctor body
  var ctorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

  // initialization in ctor body
  IR_WaLBerlaInitFunctionCollection.functions foreach(f =>
    ctorBody += IR_FunctionCall(IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype)))

  // dtor body
  var dtorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

  IR_WaLBerlaDeInitFunctionCollection.functions foreach(f =>
    dtorBody += IR_FunctionCall(IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype)))
}
