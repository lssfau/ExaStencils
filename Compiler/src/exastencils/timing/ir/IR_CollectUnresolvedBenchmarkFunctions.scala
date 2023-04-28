package exastencils.timing.ir

import scala.collection.mutable

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.util.ir.IR_ResolveBenchmarkFunctions

object IR_CollectUnresolvedBenchmarkFunctions extends DefaultStrategy("Collect unresolved benchmark functions") {
  var benchmarkNames : mutable.HashSet[String] = mutable.HashSet()

  private def getName(arg : IR_Expression) : String = arg match {
    case strConst : IR_StringConstant => strConst.value
    case strLit : IR_StringLiteral    => strLit.value
    case arg                          => Logger.error("Unknown argument type for benchmark function: " + arg.prettyprint)
  }

  private val timerFunctionNames = List("startTimer", "stopTimer")
  private val benchmarkFunctionNames = List(IR_ResolveBenchmarkFunctions.startFunction, IR_ResolveBenchmarkFunctions.stopFunction)

  override def apply(node : Option[Node] = None) = {
    benchmarkNames.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    benchmarkNames.clear
    super.applyStandalone(node)
  }

  this += new Transformation("Collect", {
    case fctCall @ IR_FunctionCall(function : IR_TimerFunctionReference, args) if Knowledge.timer_addBenchmarkMarkers && timerFunctionNames.contains(function.name) =>
      if (args.length != 1)
        Logger.warn("Invalid parameters in " + function.name + " timer function: " + args + ". Takes single argument of type String")

      benchmarkNames += getName(args.head)

      fctCall
    case stmt @ IR_ExpressionStatement(f @ IR_FunctionCall(IR_UnresolvedFunctionReference(s : String, _), args)) if benchmarkFunctionNames.contains(s) =>
      if (args.length != 1)
        Logger.warn("Invalid parameters in " + f.name + " benchmark function: " + args + ". Takes single argument of type String")

      benchmarkNames += getName(args.head)

      stmt
  })
}
