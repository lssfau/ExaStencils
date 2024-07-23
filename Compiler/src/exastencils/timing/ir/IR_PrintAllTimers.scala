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
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.parallelization.api.mpi.MPI_AllReduce
import exastencils.parallelization.api.mpi.MPI_IV_MpiSize
import exastencils.util.ir.IR_RawPrint

/// IR_PrintAllTimers

abstract class IR_AbstractPrintAllTimers extends IR_TimerFunction {

  override var name = "printAllTimers"
  override def prettyprint_decl() : String = prettyprint

  protected def timerValue = IR_VariableAccess("timerValue", IR_DoubleDatatype)


  def genPrintTimerCode(timer : IR_TimingIV) : IR_Statement = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val timeToPrint = "getTotalTime"

    timer match {
      case _ : IR_PlainTimingIV                           => // non-leveled timer
        statements += IR_VariableDeclaration(timerValue, IR_FunctionCall(IR_TimerFunctionReference(timeToPrint, IR_DoubleDatatype, None), timer.resolveName()))

        if (Knowledge.mpi_enabled) {
          statements += MPI_AllReduce(IR_AddressOf(timerValue), timerValue.datatype, 1, "+")
          statements += IR_Assignment(timerValue, MPI_IV_MpiSize, "/=")
        }

        statements += IR_RawPrint("\"Mean mean total time for Timer " + timer.name + ":\"", "timerValue")
      case leveledTimer : IR_LeveledTimingIV => // leveled timer
        val level = leveledTimer.level
        val access = leveledTimer.accessTimerAtLevel()
        statements += IR_VariableDeclaration(timerValue, IR_FunctionCall(IR_TimerFunctionReference(timeToPrint, IR_DoubleDatatype, None), access))

        if (Knowledge.mpi_enabled) {
          statements += MPI_AllReduce(IR_AddressOf(timerValue), timerValue.datatype, 1, "+")
          statements += IR_Assignment(timerValue, MPI_IV_MpiSize, "/=")
        }

        statements += IR_RawPrint("\"Mean mean total time for Timer " + timer.name + " at level " + level + ":\"", "timerValue")
    }
    IR_Scope(statements)
  }
}

case class IR_PrintAllTimers() extends IR_AbstractPrintAllTimers {
  override def generateFct() = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers

    val body = timers.toList.sortBy(_._1).map(t => genPrintTimerCode(t._2)).to[ListBuffer]

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


  // adds additional code for accumlator addition
  override def genPrintTimerCode(timer : IR_TimingIV) : IR_Statement = {
    val scope = super.genPrintTimerCode(timer).asInstanceOf[IR_Scope]
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


  override def generateFct() = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers
    val sortedAccumulators = accumulators.toList.sortBy(_._1)

    val body : ListBuffer[IR_Statement] = ListBuffer()
    body ++= sortedAccumulators.map { case (_, vAcc) => IR_VariableDeclaration(vAcc, 0.0) }
    body ++= timers.toList.sortBy(_._1).map(t => genPrintTimerCode(t._2)).to[ListBuffer]

    // print out mean total timers for automatic function timers
    def rawPrint(msg : String, expr : IR_Expression) = IR_RawPrint(ListBuffer[IR_Expression](IR_StringConstant(msg), expr))
    for (((category, optionalLevel), vAcc) <- sortedAccumulators) {
      if (categoryEnabled(category)) {
        val levelDescription = if (optionalLevel.isDefined) s" at level ${optionalLevel.get}" else ""
        body += rawPrint(s"Mean mean total time for sum of ${vAcc.name}${levelDescription}:", vAcc)
      }
    }
    IR_AutomaticTimingCategory.values.toList.sortBy(_.toString).foreach(enum =>
      if (categoryEnabled(enum)) {
        body += rawPrint(s"Mean mean total time for all automatic ${enum.toString} timers:",
          filterAndReduce(sortedAccumulators, enum))
      }
    )
    body += rawPrint("Mean mean total time for all automatic timers:", sortedAccumulators.map(_._2 : IR_Expression).reduce(_ + _))

    /*  Does not work currently, since IR_RawPrint is already wrapped in mpi_check
    if (Knowledge.mpi_enabled) {
      body += IR_IfCondition(MPI_IsRootProc(), ifBody)
    } else {
      body ++= ifBody
    }
     */

    val fct = IR_PlainFunction(name, IR_UnitDatatype, body)
    fct.allowFortranInterface = false
    fct
  }

  private def filterAndReduce(sortedAccumulators : List[((Access, Option[Int]), IR_VariableAccess)], category : Access) : IR_Expression = {
    sortedAccumulators
      .filter(elem => elem._1._1 == category)
      .map(_._2 : IR_Expression)
      .reduce(_ + _)
  }
}