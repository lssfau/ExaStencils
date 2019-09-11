package exastencils.boundary.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.boundary.ir.IR_ApplyBC
import exastencils.field.l4.L4_FieldAccess
import exastencils.logger.Logger
import exastencils.operator.l4.L4_StencilFieldAccess
import exastencils.prettyprinting.PpStream

/// L4_ApplyBC

case class L4_ApplyBC(var target : L4_Access) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "apply bc to " << target

  override def progress : IR_ApplyBC = ProgressLocation {
    // TODO: extract to strategy replacing stencil field accesses with corresponding field accesses
    // TODO: warning on ignoring offset or component accesses
    target match {
      case f : L4_FieldAccess =>
        val prog = f.progress
        IR_ApplyBC(prog.field, prog.slot)

      case sf : L4_StencilFieldAccess =>
        val progField = sf.target.getProgressedObj().field
        IR_ApplyBC(progField, L4_FieldAccess.resolveSlot(progField, sf.slot))

      case _ => Logger.error("Invalid target for L4_ApplyBC: " + target)
    }
  }
}
