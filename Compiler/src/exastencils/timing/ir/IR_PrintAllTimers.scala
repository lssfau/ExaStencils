//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Statement
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.parallelization.api.mpi.MPI_AllReduce
import exastencils.parallelization.api.mpi.MPI_IV_MpiSize
import exastencils.parallelization.api.mpi.MPI_IsRootProc
import exastencils.util.ir.IR_Print
import exastencils.util.ir.IR_RawPrint

/// IR_PrintAllTimers

abstract class IR_AbstractPrintAllTimers extends IR_TimerFunction {

  override var name = "printAllTimers"
  override def prettyprint_decl() : String = prettyprint

  protected val timerValue = IR_VariableAccess("timerValue", IR_DoubleDatatype)
  protected val precisionVar = IR_VariableAccess("streamPrecision", IR_IntegerDatatype)

  protected def prepareOutputFormatting(): ListBuffer[IR_Statement] = {
    val body : ListBuffer[IR_Statement] = ListBuffer()
    body += IR_ExpressionStatement(
      if (Knowledge.field_printFieldPrecision == -1) {
        "std::cout << std::scientific"
      } else {
        s"std::cout << std::scientific << std::setprecision(${Knowledge.field_printFieldPrecision})"
      }
    )

    body += IR_VariableDeclaration(precisionVar, IR_StringLiteral("std::cout.precision()"))
    body
  }

  protected def resetOutputFormatting() : IR_Statement = {
    IR_ExpressionStatement("std::cout.flags(std::ios::fmtflags(0))")
  }

  protected def genPrintTimerCode(timer : IR_TimingIV, lengthOfLongestTimerName : Int) : IR_Statement = {
    val statements : ListBuffer[IR_Statement] = ListBuffer()

    val timeToPrint = "getTotalTime"
    val stdOut = IR_VariableAccess("std::cout", IR_UnknownDatatype)
    var text = ""
    var padding = 0
    val atLevelText = " at level "

    timer match {
      case _ : IR_PlainTimingIV                           => // non-leveled timer
        statements += IR_VariableDeclaration(timerValue, IR_FunctionCall(IR_TimerFunctionReference(timeToPrint, IR_DoubleDatatype, None), timer.resolveName()))

        text = "Mean mean total time for Timer " + timer.name + ":"
        padding = lengthOfLongestTimerName - timer.name.length + 6 + atLevelText.length + 1 // +1 for the number representing the level
      case leveledTimer : IR_LeveledTimingIV => // leveled timer
        val level = leveledTimer.level
        val access = leveledTimer.accessTimerAtLevel()
        statements += IR_VariableDeclaration(timerValue, IR_FunctionCall(IR_TimerFunctionReference(timeToPrint, IR_DoubleDatatype, None), access))

        text = "Mean mean total time for Timer " + timer.name + atLevelText + level + ":"
        padding = lengthOfLongestTimerName - timer.name.length + 6
    }

    val printStmt = IR_Print(stdOut,
      IR_StringConstant(text), IR_StringLiteral(s"std::setw(${padding} + ${precisionVar.name})"), timerValue, IR_Print.endl
    )

    if (Knowledge.mpi_enabled) {
      statements += MPI_AllReduce(IR_AddressOf(timerValue), timerValue.datatype, 1, "+")
      statements += IR_Assignment(timerValue, MPI_IV_MpiSize, "/=")
      statements += IR_IfCondition(MPI_IsRootProc(), printStmt)
    } else {
      statements += printStmt
    }

    IR_Scope(statements)
  }
}

case class IR_PrintAllTimers() extends IR_AbstractPrintAllTimers {
  override def generateFct() = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers


    val body : ListBuffer[IR_Statement] = prepareOutputFormatting()
    body += IR_RawPrint(IR_StringConstant("--------- Timer Values ---------"))
    val lengthOfLongestName = timers.keys.map(_._1).maxBy(_.length).length
    body ++= timers.toList.sortBy(_._1).map(t => genPrintTimerCode(t._2, lengthOfLongestName)).to[ListBuffer]

    body += resetOutputFormatting()

    val fct = IR_PlainFunction(name, IR_UnitDatatype, body)
    fct.allowFortranInterface = false
    fct
  }
}

case class IR_PrintAllTimersIncludingAutomatic() extends IR_AbstractPrintAllTimers {
  import IR_AutomaticTimingCategory._

  private val accumulators : Map[(Access, Option[Int]), IR_VariableAccess] = values
    // create a variable for each category and level pair and add it to a list
    .flatMap(enum =>
      if (categoryEnabled(enum)) {
        if (categoryLeveled(enum)) {
          // leveled categories need a accumulator variable for each level
          for (n <- Range.inclusive(Knowledge.minLevel, Knowledge.maxLevel))
            yield (enum, Some(n)) -> IR_VariableAccess(s"accum_${ enum.toString }_$n", IR_DoubleDatatype) : ((Access, Option[Int]), IR_VariableAccess)
        } else {
          // non leveled categories only need one accumulator variable
          List((`enum`, None) -> IR_VariableAccess(s"accum_${ `enum`.toString }", IR_DoubleDatatype))
        }
      } else {
        Seq.empty
      }
    )
    .toMap

  override def generateFct() = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers
    val sortedAccumulators = accumulators.toList.sortBy(_._1)

    val body : ListBuffer[IR_Statement] = prepareOutputFormatting()
    body ++= sortedAccumulators.map { case (_, vAcc) => IR_VariableDeclaration(vAcc, 0.0) }

    body += IR_RawPrint(IR_StringConstant("--------- Timer Values ---------"))
    val lengthOfLongestTimerName = timers.keys.map(_._1).maxBy(_.length).length
    body ++= timers.toList.sortBy(_._1).map(t => genPrintTimerCode(t._2, lengthOfLongestTimerName)).to[ListBuffer]

    // print out mean total timers for automatic function timers
    body += IR_RawPrint(IR_StringConstant("------- Aggregated Timer Values -------"))
    body += genPrintAccuCode(sortedAccumulators)

    body += resetOutputFormatting()

    val fct = IR_PlainFunction(name, IR_UnitDatatype, body)
    fct.allowFortranInterface = false
    fct
  }

  // adds additional code for accumulator addition
  override protected def genPrintTimerCode(timer : IR_TimingIV, lengthOfLongestTimerName : Int) : IR_Statement = {
    val scope = super.genPrintTimerCode(timer, lengthOfLongestTimerName).asInstanceOf[IR_Scope]
    timer match {
      case IR_IV_AutomaticTimer(_, timingCategory) =>
        val accum = accumulators((timingCategory, None))
        scope.body += IR_Assignment(accum, timerValue, "+=")
      case IR_IV_AutomaticLeveledTimer(_, timingCategory, level) =>
        val accum = accumulators((timingCategory, Some(level)))
        scope.body += IR_Assignment(accum, timerValue, "+=")
      case _ =>
    }
    scope
  }

  private def genPrintAccuCode(sortedAccumulators : List[((Access, Option[Int]), IR_VariableAccess)]) : IR_Statement = {
    val outputTuples : ListBuffer[(String, IR_Expression)] = ListBuffer() // stores output-text and -value

    for (((category, optionalLevel), vAcc) <- sortedAccumulators) {
      if (categoryEnabled(category)) {
        val levelDescription = if (optionalLevel.isDefined) s" at level ${optionalLevel.get}" else ""
        outputTuples += ((s"Mean mean total time for sum of ${vAcc.name}${levelDescription}:", vAcc))
      }
    }

    def filterAndReduce(sortedAccumulators : List[((Access, Option[Int]), IR_VariableAccess)], category : Access) : IR_Expression = {
      sortedAccumulators
        .filter(elem => elem._1._1 == category)
        .map(_._2 : IR_Expression)
        .reduce(_ + _)
    }

    IR_AutomaticTimingCategory.values.toList.sortBy(_.toString).foreach(enum =>
      if (categoryEnabled(enum)) {
        outputTuples += ((s"Mean mean total time for all automatic ${enum.toString} timers:", filterAndReduce(sortedAccumulators, enum)))
      }
    )

    outputTuples += (("Mean mean total time for all automatic timers:", sortedAccumulators.map(_._2 : IR_Expression).reduce(_ + _)))

    /*
    for (((category, optionalLevel), vAcc) <- sortedAccumulators) {
      if (categoryEnabled(category)) {
        val levelDescription = if (optionalLevel.isDefined) s" at level ${optionalLevel.get}" else ""
        val removalPadding = if (optionalLevel.isDefined) atLevelLength else 0
        statements += IR_Print(stdOut,
          IR_StringConstant(s"Mean mean total time for sum of ${vAcc.name}${levelDescription}:"),
          IR_StringLiteral(s"std::setw(${padding(vAcc.name.length) - removalPadding})"),
          vAcc,
          IR_Print.endl
        )
      }
    }

    IR_AutomaticTimingCategory.values.toList.sortBy(_.toString).foreach(enum =>
      if (categoryEnabled(enum)) {
        statements += IR_Print(stdOut,
          IR_StringConstant(s"Mean mean total time for all automatic ${enum.toString} timers:"),
          IR_StringLiteral(s"std::setw(${padding(enum.toString.length)})"),
          filterAndReduce(sortedAccumulators, enum),
          IR_Print.endl
        )
      }
    )

    statements += IR_Print(stdOut,
      IR_StringConstant("Mean mean total time for all automatic timers:"),
      IR_StringLiteral(s"std::setw(${padding("automatic".length)})"),
      sortedAccumulators.map(_._2 : IR_Expression).reduce(_ + _),
      IR_Print.endl
    )
     */

    val statements : ListBuffer[IR_Statement] = ListBuffer()
    val stdOut = IR_VariableAccess("std::cout", IR_UnknownDatatype)
    val lengthOfLongestText = outputTuples.map(_._1).maxBy(_.length).length
    def padding(text : String) = lengthOfLongestText - text.length + 8 // 8 for exponent and integer value of decimal number
    for ((text, value) <- outputTuples) {
      statements += IR_Print(stdOut,
        IR_StringConstant(text),
        IR_StringLiteral(s"std::setw(${padding(text)} + ${IR_StringLiteral(precisionVar.name)})"),
        value,
        IR_Print.endl
      )
    }

    if (Knowledge.mpi_enabled) {
      IR_IfCondition(MPI_IsRootProc(), statements)
    } else {
      IR_Scope(statements)
    }
  }
}