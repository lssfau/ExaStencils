package exastencils.visualization.ir.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.parallelization.api.mpi._

case class IR_VisItMainloop() extends IR_FuturePlainFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction = {

    val fctBody = ListBuffer[IR_Statement]()
    val whileBody = ListBuffer[IR_Statement]()
    val consoleInputBody = ListBuffer[IR_Statement]()

    val blockingDecl = IR_VariableDeclaration(IR_BooleanDatatype, "blocking", IR_TernaryCondition(IR_VariableAccess(runModeDecl), IR_BooleanConstant(false), IR_BooleanConstant(true)))
    val visitInputDecl = IR_VariableDeclaration(IR_IntegerDatatype, "visit_input")
    val commandDecl = IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "command", nullptr)

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
      IR_VariableAccess(simDoneDecl),
      IR_Break()
    )
    whileBody += blockingDecl

    val funcRef = if (Platform.targetCompiler == "MSVC") {
      IR_ExternalFunctionReference("_fileno")
    } else {
      IR_ExternalFunctionReference("fileno")
    }

    if (Knowledge.mpi_enabled) {
      whileBody += visitInputDecl
      whileBody += IR_IfCondition(
        MPI_IsRootProc.apply(),
        IR_Assignment(IR_VariableAccess(visitInputDecl), IR_FunctionCall(IR_ExternalFunctionReference("VisItDetectInput"), IR_VariableAccess("blocking", IR_IntegerDatatype), IR_FunctionCall(funcRef, IR_Native("stdin"))))
      )
      whileBody += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_AddressOf(IR_VariableAccess(visitInputDecl)), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator)
    } else {
      whileBody += IR_VariableDeclaration(IR_IntegerDatatype, "visit_input", IR_FunctionCall(IR_ExternalFunctionReference("VisItDetectInput"), IR_VariableAccess("blocking", IR_IntegerDatatype), IR_FunctionCall(funcRef, IR_Native("stdin"))))
    }

    // body of the third case of the switch statement
    consoleInputBody += commandDecl
    consoleInputBody += IR_ArrayAllocation(IR_VariableAccess(commandDecl), IR_CharDatatype, IR_IntegerConstant(1000))

    if (Knowledge.mpi_enabled) {
      consoleInputBody += IR_IfCondition(
        MPI_IsRootProc.apply(),
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisItReadConsole"), IR_IntegerConstant(1000), IR_VariableAccess(commandDecl)) Neq visitOkay,
          IR_Break()
        )
      )
      consoleInputBody += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_VariableAccess(commandDecl), IR_IntegerConstant(1000), IR_Native("MPI_CHAR"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator)
    } else {
      consoleInputBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("VisItReadConsole"), IR_IntegerConstant(1000), IR_VariableAccess(commandDecl)) Neq visitOkay,
        IR_Break()
      )
    }

    // process console inputs
    consoleInputBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess(commandDecl), IR_StringConstant("step")) EqEq IR_IntegerConstant(0),
      ListBuffer[IR_Statement](
        IR_FunctionCall(IR_LeveledInternalFunctionReference("simulate_timestep", Knowledge.maxLevel, IR_UnitDatatype)),
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
          ListBuffer[IR_Statement](
            IR_IfCondition(
              IR_VariableAccess(updatePlotsDecl),
              ListBuffer[IR_Statement](
                IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
                IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots"))))))))

    consoleInputBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess(commandDecl), IR_StringConstant("stop")) EqEq IR_IntegerConstant(0),
      IR_Assignment(IR_VariableAccess(runModeDecl), IR_BooleanConstant(false))
    )

    consoleInputBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess(commandDecl), IR_StringConstant("run")) EqEq IR_IntegerConstant(0),
      IR_Assignment(IR_VariableAccess(runModeDecl), IR_BooleanConstant(true))
    )

    consoleInputBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess(commandDecl), IR_StringConstant("switchUpdates")) EqEq IR_IntegerConstant(0),
      IR_Assignment(IR_VariableAccess(updatePlotsDecl), IR_Negation(IR_VariableAccess(updatePlotsDecl)))
    )

    // scaling: only used for curvilinear meshes
    if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) {
      val strToReal = if (Knowledge.useDblPrecision) IR_ExternalFunctionReference("std::stod") else IR_ExternalFunctionReference("std::stof")
      consoleInputBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("strstr"), IR_VariableAccess(commandDecl), IR_StringConstant("scale=")) Neq nullptr,
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

    if (Knowledge.numLevels > 1) {
      consoleInputBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess(commandDecl), IR_StringConstant("level down")) EqEq IR_IntegerConstant(0),
        ListBuffer[IR_Statement](
          IR_Assignment(IR_VariableAccess(curLevelDecl), IR_Maximum(IR_VariableAccess(curLevelDecl) - IR_IntegerConstant(1), Knowledge.minLevel)),
          IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots"))))))

      consoleInputBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess(commandDecl), IR_StringConstant("level up")) EqEq IR_IntegerConstant(0),
        ListBuffer[IR_Statement](
          IR_Assignment(IR_VariableAccess(curLevelDecl), IR_Minimum(IR_VariableAccess(curLevelDecl) + IR_IntegerConstant(1), Knowledge.maxLevel)),
          IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots"))))))
    }
    consoleInputBody += IR_ArrayFree(IR_VariableAccess(commandDecl))

    whileBody += IR_IfCondition(
      IR_VariableAccess(visitInputDecl) < IR_IntegerConstant(0), // error, stop calling VisItDetectInput
      IR_Return()
    )

    whileBody += IR_IfCondition(
      IR_VariableAccess(visitInputDecl) EqEq IR_IntegerConstant(0), // VisItDetectInput timed out
      ListBuffer[IR_Statement](
        IR_FunctionCall(IR_LeveledInternalFunctionReference("simulate_timestep", Knowledge.maxLevel, IR_UnitDatatype)),
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
            IR_IfCondition(
              IR_VariableAccess(updatePlotsDecl),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots")))))))

    whileBody += IR_IfCondition(IR_VariableAccess(visitInputDecl) EqEq IR_IntegerConstant(1), // inbound connection is being made
      IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("VisItAttemptToCompleteConnection")),
        registerCallbackFcts,
        IR_Native("std::cout << \"Visit connection failed. Error message: \" << VisItGetLastError() << std::endl")))

    whileBody += IR_IfCondition(
      IR_VariableAccess(visitInputDecl) EqEq IR_IntegerConstant(2), // viewer sent instructions
      IR_IfCondition(
        procEngineCommandFct Neq IR_BooleanConstant(true),
        ListBuffer[IR_Statement](
          IR_FunctionCall(IR_ExternalFunctionReference("VisItDisconnect")),
          IR_Assignment(IR_VariableAccess(runModeDecl), IR_BooleanConstant(true))))) // run after VisIt closes connection

    whileBody += IR_IfCondition(
      IR_VariableAccess(visitInputDecl) EqEq IR_IntegerConstant(3), // console input detected
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
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint()
}
