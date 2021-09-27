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

package exastencils.base.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatrixAccess
import exastencils.baseExt.l4.L4_ComplexAccess
import exastencils.baseExt.l4.L4_MatrixAccess
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_Reduction

// FIXME: target as access -> resolve datatype
// FIXME: op as BinOp
case class L4_Reduction(var op : String, var target : L4_Access, targetType : L4_Datatype = L4_UnknownDatatype) extends L4_Node with L4_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << "reduction ( " << op << " : " << target << " )"
  // FIXME: IR_RealDatatype
  override def progress = ProgressLocation(IR_Reduction(op,
    target match {
      case _ : L4_UnresolvedAccess => Logger.error("Performing reduction on unresolved access.")
      case _ : L4_ComplexAccess => Logger.error("Reductions for complex accesses are currently not implemented.")
      case vAcc : L4_PlainVariableAccess => vAcc.progress
      case vAcc : L4_LeveledVariableAccess => vAcc.progress
      case mAcc : L4_MatrixAccess => mAcc.progress.asInstanceOf[IR_MatrixAccess]
    }))
}
