package exastencils.boundary.l4

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.l4._
import exastencils.boundary.ir.IR_ApplyBC
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.field.l4.L4_FieldAccess
import exastencils.prettyprinting.PpStream
import exastencils.stencil.l4.L4_StencilFieldAccess

/// L4_ApplyBC

case class L4_ApplyBC(var target : L4_Access) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "apply bc to " << target << '\n'

  override def progress : IR_ApplyBC = {
    // TODO: extract to strategy replacing stencil field accesses with corresponding field accesses
    val resolvedField = target match {
      case f : L4_FieldAccess         => f.progress.fieldSelection
      case sf : L4_StencilFieldAccess => IR_FieldSelection(sf.target.getProgressedObject.field,
        sf.target.level,
        L4_FieldAccess.resolveSlot(sf.target.getProgressedObject.field, sf.slot),
        sf.arrayIndex)
    }
    IR_ApplyBC(resolvedField)
  }
}

