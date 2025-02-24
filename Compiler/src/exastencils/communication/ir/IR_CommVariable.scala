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

import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.config.Knowledge

/// IR_HasMessageDirection

trait IR_HasMessageDirection {
  def send : Boolean

  def direction : String = if (send) "Send" else "Recv"
}

/// IR_IV_CommVariable

abstract class IR_IV_CommVariable extends IR_InternalVariable(Knowledge.comm_sepDataByFragment, false, Knowledge.comm_useFieldArrays, Knowledge.comm_useLevelArrays, Knowledge.comm_useNeighborArrays) {
  override def usesFragmentArrays : Boolean = Knowledge.comm_useFragmentArrays
  override def usesDomainArrays : Boolean = Knowledge.comm_useDomainArrays
  override def usesFieldArrays : Boolean = Knowledge.comm_useFieldArrays
  override def usesLevelArrays : Boolean = Knowledge.comm_useLevelArrays
  override def usesNeighborArrays : Boolean = Knowledge.comm_useNeighborArrays
}
