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

trait IR_TimingIV extends IR_Access {
  def name : String
  def stripName : String = name.replaceAll("[^a-zA-Z0-9]", "_")
}

trait IR_NoLevelTimingIV extends IR_UnduplicatedVariable with IR_TimingIV {
  override def resolveName() = s"timer_${stripName}"
  override def resolveDatatype() : IR_SpecialDatatype = IR_SpecialDatatype("StopWatch")

  override def getCtor() : Option[IR_Statement] = {
    Some(IR_Assignment(
      IR_MemberAccess(IR_VariableAccess(resolveName(), resolveDatatype()), "timerName"),
      IR_StringConstant(stripName)))
  }
}

trait IR_LeveledTimingIV extends IR_InternalVariable with IR_TimingIV {
  def level : Int

  override def resolveName() = s"timer_${stripName}_${level}"
  override def resolveDatatype() : IR_SpecialDatatype = IR_SpecialDatatype("StopWatch")

  // todo this might be wrong, check again
  override def getCtor() : Option[IR_Statement] = {
    Some(IR_Assignment(
      IR_MemberAccess(IR_VariableAccess(resolveName(), resolveDatatype()), "timerName"),
      IR_StringConstant(stripName)))
  }
}

case class IR_IV_Timer(var name : String) extends IR_NoLevelTimingIV

case class IR_IV_LeveledTimer(var name : String, var level : Int) extends IR_LeveledTimingIV