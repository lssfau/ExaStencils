package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.waLBerla.ir.IR_WaLBerlaUtil.make_unique

// store context

case class IR_WaLBerlaInterfaceGenerationContext(var functions : ListBuffer[IR_WaLBerlaFunction]) {

  IR_WaLBerlaCollectAccessedFields.applyStandalone(functions)
  val wbFieldAccesses = IR_WaLBerlaCollectAccessedFields.wbFieldAccesses
  val uniqueWbFields = wbFieldAccesses.groupBy(_.name).map(_._2.head.field).to[ListBuffer] // find unique wb fields
    .sortBy(_.name)
  val uniqueWbFieldsPerLevel = wbFieldAccesses.groupBy(_.field.codeName).map(_._2.head.field).to[ListBuffer] // find unique wb fields per level
    .sortBy(_.level).sortBy(_.name)


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
  if (true) { // TODO: only generate when necessary
    for (wbf <- uniqueWbFields) {
      val commScheme = IR_WaLBerlaCommScheme(wbf, slot = 0)
      members += commScheme.baseAccess()
    }
  }

  // ctor body
  var ctorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

  // add pack info for each comm scheme
  if (true) { // TODO: only generate when necessary
    // init comm scheme array
    for (wbf <- uniqueWbFieldsPerLevel) {
      val slotIt = IR_VariableAccess("slotIt", IR_IntegerDatatype)
      val commScheme  = IR_WaLBerlaCommScheme(wbf, slotIt)

      ctorBody += IR_ForLoop(IR_VariableDeclaration(slotIt, 0), slotIt < wbf.numSlots, IR_PreIncrement(slotIt),
        IR_Assignment(commScheme.resolveAccess(), make_unique(commScheme.basetype.resolveBaseDatatype.prettyprint, blockForest)))
    }

    // add pack info
    for (wbf <- uniqueWbFieldsPerLevel) {
      val slotIt = IR_VariableAccess("slotIt", IR_IntegerDatatype)
      val commScheme  = IR_WaLBerlaCommScheme(wbf, slotIt)

      ctorBody += IR_ForLoop(IR_VariableDeclaration(slotIt, 0), slotIt < wbf.numSlots, IR_PreIncrement(slotIt),
        commScheme.addPackInfo() : IR_Statement)
    }
  }
}
