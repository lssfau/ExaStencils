package exastencils.base.ir

import exastencils.polyhedron.IR_PolyScalarAccessLike
import exastencils.prettyprinting._

/// IR_VariableAccess

object IR_VariableAccess {
  def apply(decl : IR_VariableDeclaration) = new IR_VariableAccess(decl.name, decl.datatype)
}

case class IR_VariableAccess(var name : String, var datatype : IR_Datatype) extends IR_Access with IR_PolyScalarAccessLike {
  override def prettyprint(out : PpStream) : Unit = out << name
  override def uniqueID : String = name
}
