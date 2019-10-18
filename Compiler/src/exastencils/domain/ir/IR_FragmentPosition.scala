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
import exastencils.polyhedron.IR_PolyScalarAccessLike
import exastencils.prettyprinting.PpStream

/// IR_IV_FragmentPosition

case class IR_IV_FragmentPosition(dim : Int, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) with IR_Access {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"fragmentPos_$dim" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype() = IR_RealDatatype
  override def resolveDefValue() = Some(0.0)
}

/// IR_IV_FragmentPositionBegin

case class IR_IV_FragmentPositionBegin(dim : Int, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) with IR_Access with IR_PolyScalarAccessLike {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"fragmentPosBegin_$dim" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype() = IR_RealDatatype
  override def resolveDefValue() = Some(0.0)

  override def uniqueID : String = resolveName()
}

/// IR_IV_FragmentPositionEnd

case class IR_IV_FragmentPositionEnd(dim : Int, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) with IR_Access with IR_PolyScalarAccessLike {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"fragmentPosEnd_$dim" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype() = IR_RealDatatype
  override def resolveDefValue() = Some(0.0)

  override def uniqueID : String = resolveName()
}
