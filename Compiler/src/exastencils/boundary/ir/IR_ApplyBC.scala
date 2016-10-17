package exastencils.boundary.ir

import exastencils.base.ir.IR_Statement
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.prettyprinting.PpStream

/// IR_ApplyBC

case class IR_ApplyBC(var field : IR_FieldSelection) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
}
