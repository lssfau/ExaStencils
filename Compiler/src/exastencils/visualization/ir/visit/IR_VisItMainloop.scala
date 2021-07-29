package exastencils.visualization.ir.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.parallelization.api.mpi._

case class IR_VisItMainloop() extends IR_FuturePlainVisItFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction = {

    val fctBody = ListBuffer[IR_Statement]()
    val whileBody = ListBuffer[IR_Statement]()
    val consoleInputBody = ListBuffer[IR_Statement]()

    val blocking = IR_VariableAccess("blocking", IR_BooleanDatatype)
    val visitInput = IR_VariableAccess("visit_input", IR_IntegerDatatype)
    val command = IR_VariableAccess("command", IR_PointerDatatype(IR_CharDatatype))

    val registerCallbackFcts = ListBuffer[IR_Statement]()
    val procEngineCommandFct = if (Knowledge.mpi_enabled) {
      IR_FunctionCall(IR_ExternalFunctionReference("ProcessVisItCommand"))
    } else {
      IR_FunctionCall(IR_ExternalFunctionReference("VisItProcessEngineCommand"))
    }

    // callback functions to register
    if (Knowledge.mpi_enabled) {
      registerCallbackFcts += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetSlaveProcessCallback"), IR_Native("slave_process_callback"))
      registerCallbackFcts += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetGetDomainList"), IR_Native("SimGetDomainList"), nullptr)
    }
    registerCallbackFcts += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetGetMetaData"), IR_Native("SimGetMetaData"), nullptr)
    registerCallbackFcts += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetGetMesh"), IR_Native("SimGetMesh"), nullptr)
    if (Knowledge.dimensionality > 1) { // 1d variables are pictured as meshes
      registerCallbackFcts += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetGetVariable"), IR_Native("SimGetVariable"), nullptr)
    }

    registerCallbackFcts += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetCommandCallback"), IR_Native("ControlCommandCallback"), nullptr)

    // body of the while loop containing the switch statement
    whileBody += IR_IfCondition(
      simDone,
      IR_Break()
    )
    whileBody += IR_VariableDeclaration(blocking, IR_TernaryCondition(runMode, IR_BooleanConstant(false), IR_BooleanConstant(true)))

    val funcRef = if (Platform.targetCompiler == "MSVC") {
      IR_ExternalFunctionReference("_fileno")
    } else {
      IR_ExternalFunctionReference("fileno")
    }

    if (Knowledge.mpi_enabled) {
      whileBody += IR_VariableDeclaration(visitInput)
      whileBody += IR_IfCondition(
        MPI_IsRootProc.apply(),
        IR_Assignment(visitInput, IR_FunctionCall(IR_ExternalFunctionReference("VisItDetectInput"), blocking, IR_FunctionCall(funcRef, IR_Native("stdin"))))
      )
      whileBody += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_AddressOf(visitInput), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator)
    } else {
      whileBody += IR_VariableDeclaration(visitInput, IR_FunctionCall(IR_ExternalFunctionReference("VisItDetectInput"), blocking, IR_FunctionCall(funcRef, IR_Native("stdin"))))
    }

    // body of the third case of the switch statement
    consoleInputBody += IR_VariableDeclaration(command, nullptr)
    consoleInputBody += IR_ArrayAllocation(command, IR_CharDatatype, IR_IntegerConstant(1000))

    if (Knowledge.mpi_enabled) {
      consoleInputBody += IR_IfCondition(
        MPI_IsRootProc.apply(),
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisItReadConsole"), IR_IntegerConstant(1000), command) Neq visitOkay,
          IR_Break()
        )
      )
      consoleInputBody += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), command, IR_IntegerConstant(1000), IR_Native("MPI_CHAR"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator)
    } else {
      consoleInputBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("VisItReadConsole"), IR_IntegerConstant(1000), command) Neq visitOkay,
        IR_Break()
      )
    }

    // process console inputs
    consoleInputBody += IR_IfCondition(
      stringEquals(command, "step"),
      ListBuffer[IR_Statement](
        IR_FunctionCall(IR_LeveledInternalFunctionReference("simulate_timestep", Knowledge.maxLevel, IR_UnitDatatype)),
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
          ListBuffer[IR_Statement](
            IR_IfCondition(
              updatePlots,
              ListBuffer[IR_Statement](
                IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
                IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots"))))))))

    consoleInputBody += IR_IfCondition(
      stringEquals(command, "stop"),
      IR_Assignment(runMode, IR_BooleanConstant(false))
    )

    consoleInputBody += IR_IfCondition(
      stringEquals(command, "run"),
      IR_Assignment(runMode, IR_BooleanConstant(true))
    )

    consoleInputBody += IR_IfCondition(
      stringEquals(command, "switchUpdates"),
      IR_Assignment(updatePlots, IR_Negation(updatePlots))
    )

    // scaling: only used for curvilinear meshes
    if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) {
      val strToReal = if (Knowledge.useDblPrecision) IR_ExternalFunctionReference("std::stod") else IR_ExternalFunctionReference("std::stof")
      consoleInputBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("strstr"), command, IR_StringConstant("scale=")) Neq nullptr,
        IR_Native(
          s"""|try{
              |scale = $strToReal(command+6);
              |} catch (const std::invalid_argument&) {
              |
              |} catch (const std::out_of_range&) {
              |
              |}
          """.stripMargin
        )
      )
    }

    if (isMultiLeveled) {
      consoleInputBody += IR_IfCondition(
        stringEquals(command, "level down"),
        ListBuffer[IR_Statement](
          IR_Assignment(curLevel, IR_Maximum(curLevel - IR_IntegerConstant(1), Knowledge.minLevel)),
          IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots"))))))

      consoleInputBody += IR_IfCondition(
        stringEquals(command, "level up"),
        ListBuffer[IR_Statement](
          IR_Assignment(curLevel, IR_Minimum(curLevel + IR_IntegerConstant(1), Knowledge.maxLevel)),
          IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots"))))))
    }
    consoleInputBody += IR_ArrayFree(command)

    whileBody += IR_IfCondition(
      visitInput < IR_IntegerConstant(0), // error, stop calling VisItDetectInput
      IR_Return()
    )

    whileBody += IR_IfCondition(
      visitInput EqEq IR_IntegerConstant(0), // VisItDetectInput timed out
      ListBuffer[IR_Statement](
        IR_FunctionCall(IR_LeveledInternalFunctionReference("simulate_timestep", Knowledge.maxLevel, IR_UnitDatatype)),
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
            IR_IfCondition(
              updatePlots,
              IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots")))))))

    whileBody += IR_IfCondition(visitInput EqEq IR_IntegerConstant(1), // inbound connection is being made
      IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("VisItAttemptToCompleteConnection")),
        registerCallbackFcts,
        IR_Native("std::cout << \"Visit connection failed. Error message: \" << VisItGetLastError() << std::endl")))

    whileBody += IR_IfCondition(
      visitInput EqEq IR_IntegerConstant(2), // viewer sent instructions
      IR_IfCondition(
        procEngineCommandFct Neq IR_BooleanConstant(true),
        ListBuffer[IR_Statement](
          IR_FunctionCall(IR_ExternalFunctionReference("VisItDisconnect")),
          IR_Assignment(runMode, IR_BooleanConstant(true))))) // run after VisIt closes connection

    whileBody += IR_IfCondition(
      visitInput EqEq IR_IntegerConstant(3), // console input detected
      consoleInputBody
    )

    fctBody += IR_WhileLoop(
      IR_IntegerConstant(1),
      whileBody
    )

    IR_PlainFunction(
      name,
      IR_UnitDatatype,
      fctBody
    )


  }
  override def name : String = "visit_mainloop"
}
