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

package exastencils.domain.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.prettyprinting.PpStream

/// IR_IV_IterationOffsetBegin

case class IR_IV_IterationOffsetBegin(var dim : Int, var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, true, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"iterationOffsetBegin_$dim" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(1)
  def resolveAccess = super.resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, IR_NullExpression)
}

/// IR_IV_IterationOffsetEnd

case class IR_IV_IterationOffsetEnd(var dim : Int, var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, true, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"iterationOffsetEnd_$dim" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(-1)
}
