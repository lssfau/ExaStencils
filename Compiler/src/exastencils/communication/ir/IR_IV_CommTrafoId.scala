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
import exastencils.baseExt.ir.IR_LoopOverDomains
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_LoopOverNeighbors
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_FragmentConnection
import exastencils.prettyprinting.PpStream

/// IR_IV_CommTrafoId

case class IR_IV_CommTrafoId(
    var domain : IR_Expression,
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)
  override def resolveName() = s"commTrafoId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def baseDatatype = IR_IntegerDatatype
  override def resolveDefValue() = Some(-1)
}

/// IR_IV_NeighFragId

case class IR_IV_NeighFragId(
    var domain : IR_Expression,
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)
  override def resolveName() = s"neighFragId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def baseDatatype = IR_IntegerDatatype
  override def resolveDefValue() = Some(-1)
}

/// IR_IV_CommNeighIdx
// Index of the neighbor pointing back to the current fragment from the neighboring fragment
case class IR_IV_CommNeighNeighIdx(
    var domain : IR_Expression,
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {

  override def getCtor() : Option[IR_Statement] = {
    if (Knowledge.refinement_enabled) {
      def proxy(idxOfRefinedNeigh : Int) = IR_IV_CommNeighNeighIdx(domain, neighIdx, Some(idxOfRefinedNeigh), fragmentIdx)

      Some(wrapInLoops(
        IR_Scope(
          (0 until Knowledge.refinement_maxFineNeighborsForCommAxis).map(i =>
            IR_Assignment(proxy(i).resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_NullExpression, IR_NullExpression, IR_LoopOverNeighbors.defIt),
              resolveDefValue().get)
          ) : _*
        )))
    } else {
      super.getCtor()
    }
  }

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)
  override def resolveName() = s"commNeighIdx" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def baseDatatype = IR_IntegerDatatype
  override def resolveDefValue() = Some(-1)
}

