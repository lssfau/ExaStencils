package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._

// store context

case class IR_WaLBerlaInterfaceGenerationContext(var functions : ListBuffer[IR_WaLBerlaFunction]) {

  val uniqueWbFields = IR_WaLBerlaFieldCollection.objects.groupBy(_.name).map(_._2.head).to[ListBuffer] // find unique wb fields
    .sortBy(_.name)

  // members
  var publicMembers : ListBuffer[IR_VariableAccess] = ListBuffer()
  var privateMembers : ListBuffer[IR_VariableAccess] = ListBuffer()

  // block data IDs and params of waLBerla function
  val blockDataIDs = uniqueWbFields.map(wbf => IR_WaLBerlaBlockDataID(wbf, slot = 0))
  publicMembers ++= blockDataIDs.map(_.member)

  // block storage shared_ptr
  val blockForest = IR_WaLBerlaBlockForest()
  publicMembers += blockForest.member

  // comm scheme for each field. packed with a uniform pack info
  if (IR_WaLBerlaUtil.initCommSchemes) {
    for (wbf <- uniqueWbFields) {
      val commScheme = IR_WaLBerlaCommScheme(wbf, slot = 0)
      privateMembers += commScheme.baseAccess()
    }
  }

  var constructors : ListBuffer[IR_Constructor] = ListBuffer()
  var destructors : ListBuffer[IR_Destructor] = ListBuffer()

  // ctor #1: user passes blockDataIDs, blockforest
  {
    // params
    var ctorParams : ListBuffer[IR_FunctionArgument] = ListBuffer()
    ctorParams ++= blockDataIDs.map(_.ctorParameter)
    ctorParams += blockForest.ctorParameter

    // initializer list
    var ctorInitializerList : IR_MemberInitializerList = IR_MemberInitializerList()
    for (dataId <- blockDataIDs)
      ctorInitializerList.addEntry(dataId.initializerListEntry)
    ctorInitializerList.addEntry(blockForest.initializerListEntry)

    // body
    val ctorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

    // initialization in ctor body
    IR_WaLBerlaInitFunctionCollection.functions foreach (f =>
      ctorBody += IR_FunctionCall(IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype)))

    constructors += IR_Constructor(IR_WaLBerlaInterface.interfaceName, ctorParams, ctorInitializerList, ctorBody)
  }

  // ctor #2: empty parameter & initializer list, execute static convenience functions to obtain blockDataIDs and blockForest
  {
    // body
    val ctorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

    ctorBody += IR_FunctionCall(IR_WaLBerlaInitBlockForest().name)
    for (dataId <- blockDataIDs)
      ctorBody += IR_FunctionCall(IR_WaLBerlaAddFieldToStorage(dataId.wbField).name, blockForest, 0.0)

    // initialization in ctor body
    IR_WaLBerlaInitFunctionCollection.functions foreach (f =>
      ctorBody += IR_FunctionCall(IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype)))

    constructors += IR_Constructor(IR_WaLBerlaInterface.interfaceName, ListBuffer(), IR_MemberInitializerList(), ctorBody)
  }

  // dtor body
  {
    val dtorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

    IR_WaLBerlaDeInitFunctionCollection.functions foreach (f =>
      dtorBody += IR_FunctionCall(IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype)))

    destructors += IR_Destructor(IR_WaLBerlaInterface.interfaceName, dtorBody)
  }
}
