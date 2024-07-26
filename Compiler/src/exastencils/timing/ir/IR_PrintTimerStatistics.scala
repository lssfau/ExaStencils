package exastencils.timing.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_PlainFunction
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnitDatatype
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_StdVectorDatatype_VS
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
      IR_Assignment(min, "std::numeric_limits<double>::infinity()"), // todo not sure if this works
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
    body += IR_RawPrint(IR_StringConstant("TimerName / Metric | Average | Min | Max | StdDev"))
    for (index <- 0 until numberOfTimers) {
      body += IR_RawPrint(
        IR_StringConstant(timerNames(index)),
        IR_ArrayAccess(meanVectorName, index),
        IR_ArrayAccess(minVectorName, index),
        IR_ArrayAccess(maxVectorName, index),
        IR_ArrayAccess(stdDevVectorName, index)
      )
    }
    body
  }

  override def generateFct() : IR_Function = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers

    val body : ListBuffer[IR_Statement] = ListBuffer()

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


    val fct = IR_PlainFunction(name, IR_UnitDatatype, body)
    fct.allowFortranInterface = false
    fct
  }
}
