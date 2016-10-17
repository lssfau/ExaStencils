package exastencils.base.ir

import exastencils.datastructures._
import exastencils.prettyprinting.PpStream

/// IR_BoundedScalar

case class IR_BoundedScalar(var min : Long, var max : Long, var expr : IR_Expression) extends IR_Expression {
  override def datatype = expr.datatype
  override def prettyprint(out : PpStream) : Unit = out << expr

  def expandSpecial() = expr
}

/// IR_ResolveBoundedScalar

object IR_ResolveBoundedScalar extends DefaultStrategy("Resolve BoundedScalar nodes") {
  this += new Transformation("Resolve", {
    case index : IR_BoundedScalar => index.expandSpecial()
  })
}
