package exastencils.timing.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_PlainFunction
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnitDatatype
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_StdVectorDatatype_VS
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.parallelization.api.mpi.MPI_Gather
import exastencils.parallelization.api.mpi.MPI_IV_MpiSize
import exastencils.parallelization.api.mpi.MPI_IsRootProc
import exastencils.util.ir._

case class IR_PrintTimerStatistics() extends IR_TimerFunction with IR_TimerGatherer {

  override var name = "printTimerStatistics"
  override def prettyprint_decl() : String = prettyprint

  def calculateAndPrint(timerNames: List[String]) : ListBuffer[IR_Statement]= {
    val body : ListBuffer[IR_Statement] = ListBuffer()

    val numberOfTimers = timerNames.size
    val meanVectorName = "timerMean"
    val maxVectorName = "timerMax"
    val minVectorName = "timerMin"
    val stdDevVectorName = "timerStdDev"

    val i = IR_VariableAccess("i", IR_IntegerDatatype)
    val j = IR_VariableAccess("j", IR_IntegerDatatype)

    // calculate mean for every timer
    body += IR_VariableDeclaration(IR_StdVectorDatatype_VS(IR_DoubleDatatype, numberOfTimers), meanVectorName)
    body += IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, numberOfTimers), IR_PreIncrement(i),
      IR_Assignment(IR_ArrayAccess(meanVectorName, i), 0),
      IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, MPI_IV_MpiSize), IR_PreIncrement(j),
        IR_Assignment(IR_ArrayAccess(meanVectorName, i), accessTimerValue(i + j * numberOfTimers), "+=")
      ),
      IR_Assignment(IR_ArrayAccess(meanVectorName, i), MPI_IV_MpiSize, "/=")
    )

    // calculate min/max for every timer
    val max = IR_VariableAccess("maxVal", IR_DoubleDatatype)
    val min = IR_VariableAccess("minVal", IR_DoubleDatatype)
    val tmp = IR_VariableAccess("tmp", IR_DoubleDatatype)

    body += IR_VariableDeclaration(tmp)
    body += IR_VariableDeclaration(max)
    body += IR_VariableDeclaration(min)
    body += IR_VariableDeclaration(IR_StdVectorDatatype_VS(IR_DoubleDatatype, numberOfTimers), maxVectorName)
    body += IR_VariableDeclaration(IR_StdVectorDatatype_VS(IR_DoubleDatatype, numberOfTimers), minVectorName)
    body += IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, numberOfTimers), IR_PreIncrement(i),
      IR_Assignment(max, 0),
      IR_Assignment(min, "std::numeric_limits<double>::infinity()"),
      IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, MPI_IV_MpiSize), IR_PreIncrement(j),
        IR_Assignment(tmp, accessTimerValue(i + j * numberOfTimers)),
        IR_IfCondition(IR_Greater(tmp, max),
          IR_Assignment(max, tmp)
        ),
        IR_IfCondition(IR_Lower(tmp, min),
          IR_Assignment(min, tmp)
        )
      ),
      IR_Assignment(IR_ArrayAccess(maxVectorName, i), max),
      IR_Assignment(IR_ArrayAccess(minVectorName, i), min),
    )

    // calculate standard deviation
    body += IR_VariableDeclaration(IR_StdVectorDatatype_VS(IR_DoubleDatatype, numberOfTimers), stdDevVectorName)
    body += IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, numberOfTimers), IR_PreIncrement(i),
      IR_Assignment(IR_ArrayAccess(stdDevVectorName, i), 0),
      IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, MPI_IV_MpiSize), IR_PreIncrement(j),
        IR_Assignment(tmp, accessTimerValue(i + j * numberOfTimers) - IR_ArrayAccess(meanVectorName, i)),
        IR_Assignment(IR_ArrayAccess(stdDevVectorName, i), tmp * tmp, "+=")
      ),
      IR_Assignment(tmp, IR_ArrayAccess(stdDevVectorName, i) / MPI_IV_MpiSize),
      IR_Assignment(IR_ArrayAccess(stdDevVectorName, i), IR_FunctionCall(IR_MathFunctionReference("sqrt", IR_DoubleDatatype), tmp))
    )

    // print calculated values
    val stdOut = IR_VariableAccess("std::cout", IR_UnknownDatatype)

    val firstColumnText = "TimerName / Metric "
    val maxNameLength = timerNames.maxBy(_.length).length

    body += IR_ExpressionStatement(
      if (Knowledge.field_printFieldPrecision == -1) {
        "std::cout << std::scientific"
      } else {
        s"std::cout << std::scientific << std::setprecision(${Knowledge.field_printFieldPrecision})"
      }
    )

    val precisionVar = IR_VariableAccess("streamPrecision", IR_IntegerDatatype)
    body += IR_VariableDeclaration(precisionVar, IR_StringLiteral("std::cout.precision()"))

    val firstColumnWidth = IR_StringLiteral(s"std::setw(${Math.max(firstColumnText.length, maxNameLength + 1)})")
    val columnWidthBaseValue = 6 + 8 // width of largest column 'Average', which is the largest + 8 for exponent and integer part of number
    val columnWidth = IR_StringLiteral(s"std::setw(${columnWidthBaseValue} + ${precisionVar.name})")
    val columnSeparator = IR_StringConstant("|")

    body += IR_ExpressionStatement("std::cout << std::left")

    body += IR_Print(stdOut,
      firstColumnWidth, IR_StringConstant(firstColumnText), columnSeparator,
      columnWidth, IR_StringConstant(" Average "), columnSeparator,
      columnWidth, IR_StringConstant(" Min "), columnSeparator,
      columnWidth, IR_StringConstant(" Max "), columnSeparator,
      columnWidth, IR_StringConstant(" StdDev "), IR_Print.endl
      //"TimerName / Metric | Average | Min | Max | StdDev"
    )

    for (index <- 0 until numberOfTimers) {
      body += IR_Print(stdOut,
        firstColumnWidth, IR_StringConstant(timerNames(index)), columnSeparator,
        columnWidth, IR_ArrayAccess(meanVectorName, index), columnSeparator,
        columnWidth, IR_ArrayAccess(minVectorName, index), columnSeparator,
        columnWidth, IR_ArrayAccess(maxVectorName, index), columnSeparator,
        columnWidth, IR_ArrayAccess(stdDevVectorName, index), IR_Print.endl
      )
    }

    body += IR_ExpressionStatement("std::cout.flags(std::ios::fmtflags(0))")

    body
  }

  override def generateFct() : IR_Function = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers

    val body : ListBuffer[IR_Statement] = ListBuffer()
    if (timers.nonEmpty) {
      val timerNames = timers.values.map {
        case plainTimer : IR_PlainTimingIV     =>
          plainTimer.name
        case leveledTimer : IR_LeveledTimingIV =>
          leveledTimer.name + "_" + leveledTimer.level
      }.toList.sorted

      body += IR_IfCondition(MPI_IsRootProc(),
        ListBuffer[IR_Statement](
          createTimerValueStorage(MPI_IV_MpiSize * timers.size))
          ++ genDataCollect(timers, includeMeanTime = false)
          ++ ListBuffer[IR_Statement](MPI_Gather(timerValuesPointer, IR_DoubleDatatype, timers.size))
          ++ calculateAndPrint(timerNames),
        ListBuffer[IR_Statement](createTimerValueStorage(timers.size))
          ++ genDataCollect(timers, includeMeanTime = false)
          ++ ListBuffer[IR_Statement](MPI_Gather(timerValuesPointer, timerValuesPointer, IR_DoubleDatatype, timers.size))
      )
    }

    val fct = IR_PlainFunction(name, IR_UnitDatatype, body)
    fct.allowFortranInterface = false
    fct
  }
}
