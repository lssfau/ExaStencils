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

package exastencils.communication.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.field.ir.IR_Field
import exastencils.prettyprinting._

/// IR_IV_RemoteReqOutstanding

case class IR_IV_RemoteReqOutstanding(
    var field : IR_Field,
    var send : Boolean,
    var neighIdx : IR_Expression,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[Int],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_CommVariable with IR_HasMessageDirection {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName() = s"remoteReqOutstanding_${ direction }_${ concurrencyId }" +
    (if (indexOfRefinedNeighbor.isDefined) s"_${ indexOfRefinedNeighbor.get }" else "") +
    resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype() = IR_BooleanDatatype
  override def resolveDefValue() = Some(false)
}

/// IR_IV_RemoteReqOutstandingNoField

case class IR_IV_RemoteReqOutstandingNoField(
    var send : Boolean,
    var neighIdx : IR_Expression,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[Int],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_InternalVariable(true, false, false, false, true) with IR_HasMessageDirection {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName() = s"remoteReqOutstandingNoField_${ direction }_${ concurrencyId }" +
    (if (indexOfRefinedNeighbor.isDefined) s"_${ indexOfRefinedNeighbor.get }" else "") +
    resolvePostfix(fragmentIdx.prettyprint, "", "", "", neighIdx.prettyprint)
  override def resolveDatatype() = IR_BooleanDatatype
  override def resolveDefValue() = Some(false)
}

