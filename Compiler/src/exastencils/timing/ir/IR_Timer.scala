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

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.baseExt.ir.IR_UnduplicatedVariable
import exastencils.logger.Logger
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverLevels
import exastencils.prettyprinting.PpStream

/// IR_IV_Timer

object IR_IV_Timer {
  def apply(name : IR_Expression) = new IR_IV_Timer(name match {
    case IR_StringConstant(value) => value
    case IR_StringLiteral(value)  => value
    case otherExpression          =>
      Logger.warn("Timer name is not constant: " + otherExpression)
      otherExpression.prettyprint()
  })
}

object IR_IV_LeveledTimer {
  def apply(name : IR_Expression, level : Int) = new IR_IV_LeveledTimer(name match {
    case IR_StringConstant(value) => value
    case IR_StringLiteral(value)  => value
    case otherExpression          =>
      Logger.warn("Timer name is not constant: " + otherExpression)
      otherExpression.prettyprint()
  }, level)
}

trait IR_TimingIV extends IR_InternalVariable with IR_Access {
  def name : String
  def stripName : String = name.replaceAll("[^a-zA-Z0-9]", "_")

  override def resolveName() = s"timer_${stripName}"
  override def resolveDatatype() : IR_SpecialDatatype = IR_SpecialDatatype("StopWatch")
}

trait IR_PlainTimingIV extends IR_UnduplicatedVariable with IR_TimingIV {
  override def getCtor() : Option[IR_Statement] = {
    Some(IR_Assignment(
      IR_MemberAccess(IR_VariableAccess(resolveName(), resolveDatatype()), "timerName"),
      IR_StringConstant(stripName)))
  }
}

trait IR_LeveledTimingIV extends IR_TimingIV {
  def level : Int

  def accessTimerAtLevel() : IR_Access = checked_cast(resolveAccess(IR_VariableAccess(resolveName(), resolveDatatype()), IR_NullExpression, IR_NullExpression, IR_NullExpression, level, IR_NullExpression))

  protected def checked_cast(leveledTimerAccess : IR_Expression) : IR_Access = {
    leveledTimerAccess match {
      case access : IR_Access =>
        access
      case _                  => {
        Logger.error("Unexpected error in IR_IV_LeveledTimer")
        throw new Exception("Failed access of leveled timer")
      } // should never occur
    }
  }

  override def getCtor() : Option[IR_Statement] = {
    val accessViaIndex = resolveAccess(IR_VariableAccess(resolveName(), resolveDatatype()), IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_LoopOverLevels.defIt, IR_NullExpression)
    Some(wrapInLoops(IR_Assignment(
      IR_MemberAccess(checked_cast(accessViaIndex), "timerName"),  IR_StringConstant(stripName)
    )))
  }

  override def prettyprint(out : PpStream) : Unit = out << accessTimerAtLevel()
}

case class IR_IV_Timer(var name : String) extends IR_PlainTimingIV

case class IR_IV_LeveledTimer(var name : String, var level : Int) extends IR_InternalVariable(false, false, false, true, false) with IR_LeveledTimingIV