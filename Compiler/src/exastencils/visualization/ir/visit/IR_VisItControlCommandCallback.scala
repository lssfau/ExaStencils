package exastencils.visualization.ir.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge

/// IR_VisItControlCommandCallback
// implement functionality for the control buttons on the GUI

case class IR_VisItControlCommandCallback() extends IR_FuturePlainFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()

    fctBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("cmd", IR_ConstPointerDatatype(IR_CharDatatype)), IR_StringConstant("step")) EqEq IR_IntegerConstant(0),
      ListBuffer[IR_Statement](
        IR_FunctionCall(IR_LeveledInternalFunctionReference("simulate_timestep", Knowledge.maxLevel, IR_UnitDatatype)),
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
          ListBuffer[IR_Statement](
            IR_IfCondition(
              IR_VariableAccess(updatePlotsDecl),
              ListBuffer[IR_Statement](
                IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
                IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots")))))))
    )
    fctBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("cmd", IR_ConstPointerDatatype(IR_CharDatatype)), IR_StringConstant("stop")) EqEq IR_IntegerConstant(0),
      IR_Assignment(IR_VariableAccess(runModeDecl), IR_BooleanConstant(false))
    )
    fctBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("cmd", IR_ConstPointerDatatype(IR_CharDatatype)), IR_StringConstant("run")) EqEq IR_IntegerConstant(0),
      IR_Assignment(IR_VariableAccess(runModeDecl), IR_BooleanConstant(true))
    )
    fctBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("cmd", IR_ConstPointerDatatype(IR_CharDatatype)), IR_StringConstant("switchUpdates")) EqEq IR_IntegerConstant(0),
      IR_Assignment(IR_VariableAccess(updatePlotsDecl), IR_Negation(IR_VariableAccess(updatePlotsDecl)))
    )
    // only register level switches when necessary
    if (Knowledge.numLevels > 1) {
      fctBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("cmd", IR_ConstPointerDatatype(IR_CharDatatype)), IR_StringConstant("level down")) EqEq IR_IntegerConstant(0),
        ListBuffer[IR_Statement](
          IR_Assignment(IR_VariableAccess(curLevelDecl), IR_Maximum(IR_VariableAccess(curLevelDecl) - IR_IntegerConstant(1), Knowledge.minLevel)),
          IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots")))))
      )
      fctBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("cmd", IR_ConstPointerDatatype(IR_CharDatatype)), IR_StringConstant("level up")) EqEq IR_IntegerConstant(0),
        ListBuffer[IR_Statement](
          IR_Assignment(IR_VariableAccess(curLevelDecl), IR_Minimum(IR_VariableAccess(curLevelDecl) + IR_IntegerConstant(1), Knowledge.maxLevel)),
          IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots")))))
      )
    }

    IR_PlainFunction(
      name,
      IR_UnitDatatype,
      ListBuffer(IR_FunctionArgument("cmd", IR_SpecialDatatype("const char*")), IR_FunctionArgument("args", IR_SpecialDatatype("const char*")), IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype))),
      fctBody
    )
  }

  override def name : String = "ControlCommandCallback"
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint()
}
