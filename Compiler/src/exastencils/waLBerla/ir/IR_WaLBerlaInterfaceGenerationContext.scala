package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstReferenceDatatype
import exastencils.base.ir.IR_ExpressionStatement
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_MemberFunctionCall
import exastencils.base.ir.IR_MemberInitializerList
import exastencils.base.ir.IR_NullStatement
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_BlockDataID

// store context

case class IR_WaLBerlaInterfaceGenerationContext(var functions : ListBuffer[IR_WaLBerlaFunction]) {

  IR_WaLBerlaCollectAccessedFields.applyStandalone(functions)
  var wbFieldAccesses = IR_WaLBerlaCollectAccessedFields.wbFieldAccesses
  var wbFieldNames : ListBuffer[String] = wbFieldAccesses.map(_.name)

  private def toBlockDataID(name : String) = IR_VariableAccess(name + "_ID", WB_BlockDataID)

  def blockDataIDs : Map[String, IR_FunctionArgument] = wbFieldNames.sorted.map(acc => acc -> IR_FunctionArgument(toBlockDataID(acc))).toMap

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
    for (field <- wbFieldAccesses.groupBy(_.name).map(_._2.head)) {
      val commScheme = IR_WaLBerlaUtil.commScheme(field)
      members += commScheme
      ctorInitializerList.arguments += Tuple2(commScheme, blocks)
    }
  }

  // ctor body
  var ctorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

  // add pack info for each comm scheme
  if (true) { // TODO: only generate when necessary
    for (field <- wbFieldAccesses.groupBy(_.name).map(_._2.head)) {
      val commScheme = IR_WaLBerlaUtil.commScheme(field)
      ctorBody += IR_ExpressionStatement(IR_MemberFunctionCall(
        commScheme, "addPackInfo", IR_WaLBerlaUtil.createUniformPackInfo(field, toBlockDataID(field.name))))
    }
  }
}
