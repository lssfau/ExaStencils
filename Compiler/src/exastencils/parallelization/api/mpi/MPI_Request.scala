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

package exastencils.parallelization.api.mpi

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir.IR_HasMessageDirection
import exastencils.communication.ir.IR_IV_CommVariable
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.prettyprinting.PpStream

/// MPI_Request

case class MPI_Request(
    var field : IR_FieldLike,
    var send : Boolean,
    var neighIdx : IR_Expression,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_CommVariable with IR_HasMessageDirection {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName() = s"mpiRequest_${ direction }_${ concurrencyId }" +
    resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)

  override def resolveDatatype() = "MPI_Request"
}

/// MPI_RequestNoField

case class MPI_RequestNoField(
    var send : Boolean,
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_InternalVariable(true, false, false, false, true) with IR_HasMessageDirection {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName() = s"mpiRequestNoField_$direction" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", neighIdx.prettyprint)
  override def resolveDatatype() = "MPI_Request"
}

