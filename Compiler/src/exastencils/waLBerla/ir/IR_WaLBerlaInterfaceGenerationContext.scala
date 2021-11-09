package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_BlockDataID

// store context

case class IR_WaLBerlaInterfaceGenerationContext(var functions : ListBuffer[IR_WaLBerlaFunction]) {

  IR_WaLBerlaCollectAccessedFields.applyStandalone(functions)
  var wbFieldAccesses = IR_WaLBerlaCollectAccessedFields.wbFieldAccesses

  private def toBlockDataID(field : IR_WaLBerlaField, slot : Int) =
    IR_VariableAccess(IR_WaLBerlaUtil.createBlockDataIdName(field, slot), WB_BlockDataID)

  def blockDataIDs : Map[String, IR_FunctionArgument] = wbFieldAccesses.sortBy(_.name).flatMap(acc => {
    (0 until acc.target.numSlots).map(slot => {
      val blockDataId = toBlockDataID(acc.field, slot)
      blockDataId.name -> IR_FunctionArgument(blockDataId)
    })
  }).toMap

  // ctor params and members
  var ctorParams : ListBuffer[IR_FunctionArgument] = ListBuffer()
  var members : ListBuffer[IR_VariableAccess] = ListBuffer()
  var ctorInitializerList : IR_MemberInitializerList = IR_MemberInitializerList()

  // block data IDs and params of waLBerla function
  ctorParams ++= blockDataIDs.values
  members ++= blockDataIDs.values.map(arg => IR_VariableAccess(IR_WaLBerlaUtil.getGeneratedName(arg.name), arg.datatype))

  // block storage shared_ptr
  private val blocks =
    IR_VariableAccess(IR_WaLBerlaUtil.blockForestPtr.name, IR_ConstReferenceDatatype(IR_WaLBerlaUtil.blockForestPtr.datatype))
  ctorParams += IR_FunctionArgument(blocks)
  members += IR_WaLBerlaUtil.blockForestMember

  // bundle members and ctor params together to build a member init list for the ctor
  ctorInitializerList.arguments ++= members.zip(ctorParams.map(_.access))

  // comm scheme for each field. packed with a uniform pack info
  if (true) { // TODO: only generate when necessary
    for (wbfAcc <- wbFieldAccesses.groupBy(_.name).map(_._2.head)) {

      val commScheme = IR_WaLBerlaCommScheme(wbfAcc.field)
      members += commScheme.baseAccess()
      ctorInitializerList.arguments += commScheme.ctorInitializerList(blocks)
    }
  }

  // ctor body
  var ctorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

  // add pack info for each comm scheme
  if (true) { // TODO: only generate when necessary
    for (wbfAcc <- wbFieldAccesses.groupBy(_.name).map(_._2.head)) {
      val wbField = wbfAcc.field
      val commScheme = IR_WaLBerlaCommScheme(wbField)
      ctorBody ++= (0 until commScheme.numSlots).map(slot =>
        IR_ExpressionStatement(IR_MemberFunctionCall(commScheme.resolveAccess(slot),
          "addPackInfo", IR_WaLBerlaUtil.createUniformPackInfo(wbField, IR_WaLBerlaUtil.getBlockDataID(wbField, slot)))))
    }
  }
}
