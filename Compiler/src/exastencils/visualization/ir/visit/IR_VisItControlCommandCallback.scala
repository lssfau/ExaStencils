package exastencils.visualization.ir.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.field.ir.IR_FieldCollection
import exastencils.visualization.ir.visit.IR_VisItGlobals._

/// IR_VisItControlCommandCallback
// implement functionality for the control buttons on the GUI

case class IR_VisItControlCommandCallback() extends IR_VisItFuturePlainFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()

    val cmd = IR_VariableAccess("cmd", IR_SpecialDatatype("const char*"))

    fctBody += IR_IfCondition(
      stringEquals(cmd, "step"),
      ListBuffer[IR_Statement](
        IR_FunctionCall(IR_LeveledInternalFunctionReference("simulate_timestep", Knowledge.maxLevel, IR_UnitDatatype)),
        IR_IfCondition(
          callExtFunction("VisItIsConnected"),
          ListBuffer[IR_Statement](
            IR_IfCondition(
              updatePlots,
              ListBuffer[IR_Statement](
                callExtFunction("VisItTimeStepChanged"),
                callExtFunction("VisItUpdatePlots"))))))
    )
    fctBody += IR_IfCondition(
      stringEquals(cmd, "stop"),
      IR_Assignment(runMode, IR_BooleanConstant(false))
    )
    fctBody += IR_IfCondition(
      stringEquals(cmd, "run"),
      IR_Assignment(runMode, IR_BooleanConstant(true))
    )
    fctBody += IR_IfCondition(
      stringEquals(cmd, "toggle updates"),
      IR_Assignment(updatePlots, IR_Negation(updatePlots))
    )
    // only register level switches when necessary
    if (isMultiLeveled) {
      fctBody += IR_IfCondition(
        stringEquals(cmd, "toggle level"),
        ListBuffer[IR_Statement](
          IR_Assignment(curLevel, modulo(curLevel - Knowledge.minLevel - 1, Knowledge.numLevels) + Knowledge.minLevel),
          IR_IfCondition(
            callExtFunction("VisItIsConnected"),
            ListBuffer[IR_Statement](
              callExtFunction("VisItTimeStepChanged"),
              callExtFunction("VisItUpdatePlots"))))
      )
    }
    // only register slot switch when necessary
    if (isMultiSlotted) {
      fctBody += IR_IfCondition(
        stringEquals(cmd, "toggle slot"),
        ListBuffer[IR_Statement](
          IR_Assignment(curSlot, (curSlot + 1) Mod IR_FieldCollection.objects.map(_.numSlots).max),
          IR_IfCondition(
            callExtFunction("VisItIsConnected"),
            ListBuffer[IR_Statement](
              callExtFunction("VisItTimeStepChanged"),
              callExtFunction("VisItUpdatePlots"))))
      )
    }

    IR_PlainFunction(
      name,
      IR_UnitDatatype,
      ListBuffer(IR_FunctionArgument(cmd), IR_FunctionArgument("args", IR_SpecialDatatype("const char*")), IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype))),
      fctBody
    )
  }

  override def name : String = "ControlCommandCallback"
}
