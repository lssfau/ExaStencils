package exastencils.boundary.ir

import exastencils.base.ir.IR_Statement
import exastencils.knowledge.FieldSelection
import exastencils.prettyprinting.PpStream

/// IR_ApplyBC

case class IR_ApplyBC(var field : FieldSelection) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
}
