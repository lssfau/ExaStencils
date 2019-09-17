package exastencils.util.ir

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.hack.ir.HACK_IR_UndeterminedFunctionReference
import exastencils.logger.Logger

/// IR_ResolveBenchmarkFunctions

object IR_ResolveBenchmarkFunctions extends DefaultStrategy("ResolveBenchmarkFunctions") {
  this += new Transformation("ResolveFunctionCalls", {
    case IR_ExpressionStatement(f @ IR_FunctionCall(HACK_IR_UndeterminedFunctionReference("benchmarkStart", _), args)) =>
      Knowledge.benchmark_backend match {
        case "likwid" =>
          if (1 != args.length || args.head.datatype != IR_StringDatatype)
            Logger.error("benchmarkStart takes a single argument of type String for benchmark_backend 'likwid'")

          f.function.name = "LIKWID_MARKER_START"
          IR_ExpressionStatement(f)

        case _ => IR_NullStatement
      }

    case IR_ExpressionStatement(f @ IR_FunctionCall(HACK_IR_UndeterminedFunctionReference("benchmarkStop", _), args)) =>
      Knowledge.benchmark_backend match {
        case "likwid" =>
          if (1 != args.length || args.head.datatype != IR_StringDatatype)
            Logger.error("benchmarkStop takes a single argument of type String for benchmark_backend 'likwid'")

          f.function.name = "LIKWID_MARKER_STOP"
          IR_ExpressionStatement(f)

        case _ => IR_NullStatement
      }
  })
}
