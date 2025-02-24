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
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

/// IR_IV_NeighborIsValidLike: can be matched to detect kernels for communication/boundary handling
trait IR_IV_NeighborIsValidLike {
  def domainIdx : IR_Expression
  def neighIdx : IR_Expression
  def fragmentIdx : IR_Expression
  def indexOfRefinedNeighbor : Option[IR_Expression]
}

/// IR_IV_FragmentConnection
abstract class IR_IV_FragmentConnection extends IR_InternalVariable(true, true, false, false, true) {
  override def usesFragmentArrays : Boolean = true
  override def usesDomainArrays : Boolean = true
  override def usesNeighborArrays : Boolean = true

  def baseDatatype : IR_Datatype
  override def resolveDatatype() : IR_Datatype = {
    if (Knowledge.refinement_enabled)
      IR_ArrayDatatype(baseDatatype, Knowledge.refinement_maxFineNeighborsForCommAxis)
    else
      baseDatatype
  }

  override def getCtor() : Option[IR_Statement] = {
    if (Knowledge.refinement_enabled && resolveDefValue().isDefined) {
      def resolveAccess(i : Int) : IR_Expression = {
        val access = super.resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_NullExpression, IR_NullExpression, IR_LoopOverNeighbors.defIt)

        IR_ArrayAccess(access, i)
      }

      Some(wrapInLoops(IR_Scope((0 until Knowledge.refinement_maxFineNeighborsForCommAxis).map(i => IR_Assignment(resolveAccess(i), resolveDefValue().get)) : _*)))
    } else {
      super.getCtor()
    }
  }

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    val access = super.resolveAccess(baseAccess, fragment, domain, field, level, neigh)

    if (Knowledge.refinement_enabled) IR_ArrayAccess(access, if (indexOfRefinedNeighbor.isDefined) indexOfRefinedNeighbor.get else 0) else access
  }

  def indexOfRefinedNeighbor : Option[IR_Expression]
}

/// IR_IV_NeighborIsValid

case class IR_IV_NeighborIsValid(
    var domainIdx : IR_Expression,
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection with IR_IV_NeighborIsValidLike {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domainIdx, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName() = s"neighbor_isValid" + resolvePostfix(fragmentIdx.prettyprint, domainIdx.prettyprint, "", "", neighIdx.prettyprint)
  override def baseDatatype = IR_BooleanDatatype
  override def resolveDefValue() = Some(false)
}

/// IR_IV_NeighborIsRemote

case class IR_IV_NeighborIsRemote(
    var domainIdx : IR_Expression,
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domainIdx, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName() = s"neighbor_isRemote" + resolvePostfix(fragmentIdx.prettyprint, domainIdx.prettyprint, "", "", neighIdx.prettyprint)
  override def baseDatatype = IR_BooleanDatatype
  override def resolveDefValue() = Some(false)
}

/// IR_IV_NeighborFragmentIdx

case class IR_IV_NeighborFragmentIdx(
    var domainIdx : IR_Expression,
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domainIdx, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName() = s"neighbor_fragCommId" + resolvePostfix(fragmentIdx.prettyprint, domainIdx.prettyprint, "", "", neighIdx.prettyprint)
  override def baseDatatype = IR_IntegerDatatype
  override def resolveDefValue() = Some(-1)
}

/// IR_IV_NeighborRemoteRank

case class IR_IV_NeighborRemoteRank(
    var domainIdx : IR_Expression,
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domainIdx, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName() = s"neighbor_remoteRank" + resolvePostfix(fragmentIdx.prettyprint, domainIdx.prettyprint, "", "", neighIdx.prettyprint)
  override def baseDatatype = IR_IntegerDatatype
  override def resolveDefValue() = Some("MPI_PROC_NULL")
}

