package exastencils.boundary.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.boundary.ir.IR_ApplyBC
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.field.l4.L4_FieldAccess
import exastencils.operator.l4.L4_StencilFieldAccess
import exastencils.prettyprinting.PpStream

/// L4_ApplyBC

case class L4_ApplyBC(var target : L4_Access) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "apply bc to " << target

  override def progress : IR_ApplyBC = ProgressLocation {
    // TODO: extract to strategy replacing stencil field accesses with corresponding field accesses
    // TODO: warning on ignoring offset or component accesses
    val resolvedField = target match {
      case f : L4_FieldAccess         => f.progress.fieldSelection
      case sf : L4_StencilFieldAccess => IR_FieldSelection(sf.target.getProgressedObj().field,
        sf.target.level,
        L4_FieldAccess.resolveSlot(sf.target.getProgressedObj().field, sf.slot))
    }
    IR_ApplyBC(resolvedField)
  }
}
