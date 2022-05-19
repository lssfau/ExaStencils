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

package exastencils.boundary.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.boundary.ir.IR_ApplyBC
import exastencils.field.l4.L4_FieldAccess
import exastencils.logger.Logger
import exastencils.operator.l4.L4_StencilFieldAccess
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.l4.field.L4_WaLBerlaFieldCollection

/// L4_ApplyBC

case class L4_ApplyBC(var target : L4_Access) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "apply bc to " << target

  override def progress : IR_ApplyBC = ProgressLocation {
    // TODO: extract to strategy replacing stencil field accesses with corresponding field accesses
    // TODO: warning on ignoring offset or component accesses
    target match {
      case f : L4_FieldAccess =>
        val prog = f.progress
        val field = if (L4_WaLBerlaFieldCollection.contains(f)) L4_WaLBerlaFieldCollection.getByFieldAccess(f).get.progress() else f.progress.field
        IR_ApplyBC(field, prog.slot)

      case sf : L4_StencilFieldAccess =>
        val progField = sf.target.getProgressedObj().field
        IR_ApplyBC(progField, L4_FieldAccess.resolveSlot(progField, sf.slot))

      case _ => Logger.error("Invalid target for L4_ApplyBC: " + target)
    }
  }
}
