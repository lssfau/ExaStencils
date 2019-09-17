package exastencils.util.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures._
import exastencils.hack.ir.HACK_IR_UndeterminedFunctionReference
import exastencils.parallelization.api.mpi.MPI_IsRootProc

/// IR_ResolveCharacteristicsFunctions

object IR_ResolveCharacteristicsFunctions extends DefaultStrategy("ResolveCharacteristicsFunctions") {
  this += new Transformation("ResolveFunctionCalls", {

    case IR_ExpressionStatement(IR_FunctionCall(HACK_IR_UndeterminedFunctionReference("clearCharacteristics", _), args)) =>
      var stmts = ListBuffer[IR_Statement]()

      stmts += IR_VariableDeclaration(IR_SpecialDatatype("std::ofstream"), "outFile")
      stmts += IR_MemberFunctionCall(IR_VariableAccess("outFile", IR_UnknownDatatype), "open", "\"" + Settings.characteristicsFile + "\"")
      stmts += IR_MemberFunctionCall(IR_VariableAccess("outFile", IR_UnknownDatatype), "close")

      if (Knowledge.mpi_enabled)
        IR_IfCondition(MPI_IsRootProc(), stmts)
      else
        IR_Scope(stmts)

    case IR_ExpressionStatement(IR_FunctionCall(HACK_IR_UndeterminedFunctionReference("logCharacteristics", _), args)) =>
      var stmts = ListBuffer[IR_Statement]()

      stmts += IR_VariableDeclaration(IR_SpecialDatatype("std::ofstream"), "outFile")
      stmts += IR_MemberFunctionCall(IR_VariableAccess("outFile", IR_UnknownDatatype), "open", "\"" + Settings.characteristicsFile + "\"", "std::ofstream::app")

      args.foreach(a => stmts += IR_Print(IR_VariableAccess("outFile", IR_UnknownDatatype), ListBuffer(a, IR_StringConstant(Settings.csvSeparator))))
      stmts += IR_Print(IR_VariableAccess("outFile", IR_UnknownDatatype), IR_StringConstant("\\n"))

      stmts += IR_MemberFunctionCall(IR_VariableAccess("outFile", IR_UnknownDatatype), "close")

      if (Knowledge.mpi_enabled)
        IR_IfCondition(MPI_IsRootProc(), stmts)
      else
        IR_Scope(stmts)
  })
}
