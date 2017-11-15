package exastencils.polyhedron

import exastencils.base.ir.IR_Node
import exastencils.field.ir.IR_Field

case class IR_ExternalFieldAlias(newName : String, field : IR_Field) extends IR_Node
