package exastencils.waLBerla.ir.refinement

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaPlainFunction
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaWrapperFunction

/// IR_WaLBerlaWorkloadAndMemoryAssignment

case class IR_WaLBerlaWorkloadAndMemoryAssignment() extends IR_WaLBerlaWrapperFunction {
  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {

    val sForest = IR_VariableAccess("sForest", IR_ReferenceDatatype(IR_SpecialDatatype("SetupBlockForest")))
    val sBlock = IR_VariableAccess("block", IR_SpecialDatatype("auto"))

    val body : ListBuffer[IR_Statement] = ListBuffer()

    body += IR_ForLoop(
      IR_VariableDeclaration(sBlock, IR_MemberFunctionCall(sForest, "begin")),
      IR_Neq(sBlock, IR_MemberFunctionCall(sForest, "end")),
      IR_PreIncrement(sBlock),
      ListBuffer[IR_Statement](
        IR_MemberFunctionCallArrow(sBlock, "setWorkload",
          IR_FunctionCall("numeric_cast < workload_t >", IR_LeftShift(1, IR_MemberFunctionCallArrow(sBlock, "getLevel")))),
        IR_MemberFunctionCallArrow(sBlock, "setMemory",
          IR_FunctionCall("numeric_cast < memory_t >", 1))
      )
    )

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(IR_FunctionArgument(sForest)), body)
  }
  override def isInterfaceFunction : Boolean = false
  override def inlineIncludeImplementation : Boolean = true
  override def name : String = "workloadAndMemoryAssignment"
}
