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

/// IR_MemberAccess

case class IR_MemberAccess(var base : IR_Access, var member : String) extends IR_Access {
  // FIXME: datatype
  override def datatype = base.datatype
  override def prettyprint(out : PpStream) : Unit = out << base << '.' << member
}

/// IR_MemberFunctionCall

object IR_MemberFunctionCall {
  def apply(objectName : IR_Expression, name : String, args : IR_Expression*) =
    new IR_MemberFunctionCall(objectName, name, args.to[ListBuffer])
}

case class IR_MemberFunctionCall(var objectName : IR_Expression, var name : String, var arguments : ListBuffer[IR_Expression]) extends IR_Expression {

  // FIXME: datatype
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << objectName << '.' << name << '(' <<< (arguments, ", ") << ')'
}
