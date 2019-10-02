package exastencils.util.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.hack.ir._
import exastencils.logger.Logger

/// IR_ResolvePrintWithReducedPrec

object IR_ResolvePrintWithReducedPrec extends DefaultStrategy("ResolvePrintWithReducedPrec") {
  this += new Transformation("ResolveFunctionCalls", {
    case stmt @ IR_ExpressionStatement(fctCall @ IR_FunctionCall(IR_UnresolvedFunctionReference("printWithReducedPrec", _), args)) =>
      if (1 != args.length)
        Logger.error("Malformed call to printWithReducedPrec")

      val fctCollection = IR_UserFunctions.get
      if (!fctCollection.functions.exists(_.name == "gen_printVal")) {
        def toPrint = IR_VariableAccess("toPrint", IR_RealDatatype)

        def printWithPrec(prec : Int) = {
          ListBuffer[IR_Statement](
            HACK_IR_Native(s"std::streamsize oldPrec = std::cout.precision()"),
            HACK_IR_Native(s"std::cout.precision($prec)"),
            IR_RawPrint(toPrint),
            HACK_IR_Native(s"std::cout.precision(oldPrec)"))
        }

        var precision = Knowledge.testing_maxPrecision
        var threshold = Knowledge.testing_zeroThreshold * List.fill(precision - 1)(10).product

        var body = IR_IfCondition(toPrint <= threshold,
          printWithPrec(precision - 1),
          printWithPrec(precision))
        precision -= 2
        threshold /= 10

        while (precision > 0) {
          body = IR_IfCondition(toPrint <= threshold,
            printWithPrec(precision),
            body)

          precision -= 1
          threshold /= 10
        }

        body = IR_IfCondition(toPrint <= threshold,
          IR_RawPrint(IR_StringConstant("EFFECTIVELY ZERO")),
          body
        )

        fctCollection += IR_PlainFunction("gen_printVal", IR_UnitDatatype, IR_FunctionArgument(toPrint), body)
      }

      fctCall.function = IR_PlainInternalFunctionReference("gen_printVal", IR_UnitDatatype)
      stmt
  })
}
