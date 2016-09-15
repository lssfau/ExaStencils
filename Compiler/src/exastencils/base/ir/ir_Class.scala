package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.prettyprinting.PpStream

/// IR_ObjectInstantiation

object IR_ObjectInstantiation {
  def apply(datatype : IR_Datatype, name : String, ctorArgs : IR_Expression*) =
    new IR_ObjectInstantiation(datatype, name, ctorArgs.to[ListBuffer])
}

case class IR_ObjectInstantiation(var datatype : IR_Datatype, var name : String, var ctorArgs : ListBuffer[IR_Expression]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << datatype.resolveDeclType << ' ' << name << datatype.resolveDeclPostscript
    if (ctorArgs.nonEmpty)
      out << '(' <<< (ctorArgs, ", ") << ')'
    out << ';'
  }
}
