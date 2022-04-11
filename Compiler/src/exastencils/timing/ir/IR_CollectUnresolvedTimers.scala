package exastencils.timing.ir

import scala.collection.mutable

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_StringLiteral
import exastencils.base.ir.IR_VariableAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.logger.Logger

object IR_CollectUnresolvedTimers extends DefaultStrategy("Collect all timers used") {
  var timers : mutable.HashSet[String] = mutable.HashSet()

  private def getName(arg : IR_Expression) : String = arg match {
    case vAcc : IR_VariableAccess     => vAcc.name
    case strConst : IR_StringConstant => strConst.value
    case strLit : IR_StringLiteral    => strLit.value
    case arg                          => Logger.error("Unknown argument type for benchmark function: " + arg.prettyprint)
  }

  override def apply(node : Option[Node] = None) = {
    timers.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    timers.clear
    super.applyStandalone(node)
  }

  this += new Transformation("Collect", {
    case fctCall @ IR_FunctionCall(function : IR_TimerFunctionReference, args) if List("startTimer", "stopTimer").contains(function.name) =>

      if (args.length != 1)
        Logger.warn("Invalid parameters in " + function.name + " timer function: " + args + ". Takes single argument of type String")

      timers += getName(args.head)

      fctCall
  })
}
