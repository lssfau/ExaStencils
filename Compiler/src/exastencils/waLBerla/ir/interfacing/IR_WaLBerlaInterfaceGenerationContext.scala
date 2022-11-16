package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.waLBerla.ir.blockforest._
import exastencils.waLBerla.ir.communication._
import exastencils.waLBerla.ir.cuda.IR_WaLBerlaAddGPUFieldToStorage
import exastencils.waLBerla.ir.field._
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

case class IR_WaLBerlaInterfaceGenerationContext(var functions : ListBuffer[IR_WaLBerlaFunction]) {

  val uniqueWbFields = IR_WaLBerlaFieldCollection.objects.groupBy(_.name).map(_._2.head).to[ListBuffer] // find unique wb fields
    .sortBy(_.name)

  // members
  var publicMembers : ListBuffer[IR_VariableAccess] = ListBuffer()
  var privateMembers : ListBuffer[IR_VariableAccess] = ListBuffer()

  // block data IDs and params of waLBerla function
  val blockDataIDs = uniqueWbFields.map(wbf => IR_WaLBerlaBlockDataID(wbf, slot = 0) -> IR_WaLBerlaBlockDataID(wbf, slot = 0, onGPU = true))
  publicMembers ++= blockDataIDs.map(_._1.member)
  if (Knowledge.cuda_enabled)
    publicMembers ++= blockDataIDs.map(_._2.member)

  // block storage shared_ptr
  val blockForest = IR_WaLBerlaBlockForest()
  privateMembers += blockForest.member

  // comm scheme for each field. packed with a uniform pack info
  if (IR_WaLBerlaUtil.initCommSchemes) {
    for (wbf <- uniqueWbFields) {
      val commScheme = IR_WaLBerlaCommScheme(wbf, slot = 0)
      privateMembers += commScheme.baseAccess()
    }
  }

  var constructors : ListBuffer[IR_Constructor] = ListBuffer()
  var destructors : ListBuffer[IR_Destructor] = ListBuffer()

  // prevent duplicate/unnecessary calls
  var initFunctions : ListBuffer[IR_PlainInternalFunctionReference] = Duplicate(IR_WaLBerlaInitFunctionCollection.functions)
    .map(f => IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype))

  // ctor #1: empty parameter & initializer list, execute static convenience functions to obtain blockDataIDs and blockForest
  {
    // body
    val ctorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

    // assign members to return values of init functions
    ctorBody += IR_Assignment(blockForest.member, IR_FunctionCall(IR_WaLBerlaInitBlockForest().name))
    initFunctions = initFunctions.filterNot(f => f.name == IR_WaLBerlaInitBlockForest().name)
    for ((dataIdCPU, _) <- blockDataIDs) {
      ctorBody += IR_Assignment(dataIdCPU.member, IR_FunctionCall(IR_WaLBerlaAddFieldToStorage(dataIdCPU.wbField).name, blockForest, 0.0))
      initFunctions = initFunctions.filterNot(f => f.name == IR_WaLBerlaAddFieldToStorage(dataIdCPU.wbField).name)
    }

    if (Knowledge.cuda_enabled) {
      for ((dataIDCPU, dataIDGPU) <- blockDataIDs) {
        ctorBody += IR_Assignment(dataIDGPU.member, IR_FunctionCall(IR_WaLBerlaAddGPUFieldToStorage(dataIDGPU.wbField).name, blockForest, dataIDCPU))
        initFunctions = initFunctions.filterNot(f => f.name == IR_WaLBerlaAddGPUFieldToStorage(dataIDGPU.wbField).name)
      }
    }

    // initialization in ctor body
    initFunctions foreach (f => ctorBody += IR_FunctionCall(f))

    constructors += IR_Constructor(IR_WaLBerlaInterface.interfaceName, ListBuffer(), IR_MemberInitializerList(), ctorBody)
  }

  // ctor #2: user passes blockDataIDs, blockforest
  {
    // params
    var ctorParams : ListBuffer[IR_FunctionArgument] = ListBuffer()
    ctorParams ++= (blockDataIDs.map(_._1) ++ (if (Knowledge.cuda_enabled) blockDataIDs.map(_._2) else Nil)).map(_.ctorParameter)
    ctorParams += blockForest.ctorParameter

    // initializer list
    var ctorInitializerList : IR_MemberInitializerList = IR_MemberInitializerList()
    for (dataId <- (blockDataIDs.map(_._1) ++ (if (Knowledge.cuda_enabled) blockDataIDs.map(_._2) else Nil)))
      ctorInitializerList.addEntry(dataId.initializerListEntry)
    ctorInitializerList.addEntry(blockForest.initializerListEntry)

    // body
    val ctorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

    // initialization in ctor body
    initFunctions foreach (f => ctorBody += IR_FunctionCall(f))

    constructors += IR_Constructor(IR_WaLBerlaInterface.interfaceName, ctorParams, ctorInitializerList, ctorBody)
  }

  // dtor body
  {
    val dtorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

    IR_WaLBerlaDeInitFunctionCollection.functions foreach (f =>
      dtorBody += IR_FunctionCall(IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype)))

    destructors += IR_Destructor(IR_WaLBerlaInterface.interfaceName, dtorBody)
  }
}
