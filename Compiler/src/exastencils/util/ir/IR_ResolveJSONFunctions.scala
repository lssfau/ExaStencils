package exastencils.util.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi._

/// IR_ResolveJSONFunctions

object IR_ResolveJSONFunctions extends DefaultStrategy("ResolveJSONFunctions") {

  def wrapStmtsMPI(stmts : ListBuffer[IR_Statement]) =
    if (Knowledge.mpi_enabled)
      IR_IfCondition(MPI_IsRootProc(), stmts)
    else
      IR_Scope(stmts)

  this += new Transformation("ResolveFunctionCalls", {
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printJSON", _), args)) =>

      def handleStringArg(arg : IR_Expression) : IR_StringConstant = arg match {
        case vAcc : IR_VariableAccess     =>
          IR_StringConstant(vAcc.name)
        case strConst : IR_StringConstant =>
          strConst
        case strLit : IR_StringLiteral    =>
          IR_StringConstant(strLit.value)
        case arg                          =>
          Logger.error("Unknown argument type for printJSON function: "
            + arg.prettyprint() + ". Signature is printJSON(\"filename\", \"key\", expr, \"key\", expr, ...")
      }

      val filename = handleStringArg(args.head)

      var stmts = ListBuffer[IR_Statement]()

      stmts += IR_VariableDeclaration(IR_SpecialDatatype("std::ofstream"), "outFile")
      stmts += IR_MemberFunctionCall(IR_VariableAccess("outFile", IR_UnknownDatatype), "open", filename, "std::ios::trunc")
      stmts += IR_Print(IR_VariableAccess("outFile", IR_UnknownDatatype), IR_StringConstant("{"), IR_Print.newline)

      // body
      for (entry <- 1 until args.length by 2) {
        var toPrint = ListBuffer[IR_Expression]()
        toPrint += IR_StringConstant("\t\\\"")
        toPrint += handleStringArg(args(entry))
        toPrint += "\"\\\" : \""
        toPrint += args(entry + 1)
        if (entry < args.length - 2)
          toPrint += "\",\""
        toPrint += IR_Print.newline

        stmts += IR_Print(IR_VariableAccess("outFile", IR_UnknownDatatype), toPrint : _*)
      }

      stmts += IR_Print(IR_VariableAccess("outFile", IR_UnknownDatatype), IR_StringConstant("}"), IR_Print.endl)
      stmts += IR_MemberFunctionCall(IR_VariableAccess("outFile", IR_UnknownDatatype), "close")

      wrapStmtsMPI(stmts)
  })
}
