package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.prettyprinting._

/// IR_FunctionArgument

object IR_FunctionArgument {
  // generate declaration corresponding to given access
  def apply(access : IR_VariableAccess) = new IR_FunctionArgument(access.name, access.datatype)
}

case class IR_FunctionArgument(var name : String, var datatype : IR_Datatype) extends IR_Node with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << datatype << ' ' << name

  def access = IR_VariableAccess(name, Duplicate(datatype))
}

/// IR_Function

object IR_Function {
  // empty function
  def apply(datatype : IR_Datatype, name : String) =
    new IR_Function(datatype, name, ListBuffer(), ListBuffer())

  // no function arguments
  def apply(datatype : IR_Datatype, name : String, body : ListBuffer[IR_Statement]) =
    new IR_Function(datatype, name, ListBuffer(), body)

  // single statement body
  def apply(datatype : IR_Datatype, name : String, arguments : ListBuffer[IR_FunctionArgument], body : IR_Statement) =
    new IR_Function(datatype, name, arguments, ListBuffer(body))

  // only one function argument
  def apply(datatype : IR_Datatype, name : String, arguments : IR_FunctionArgument, body : ListBuffer[IR_Statement]) =
    new IR_Function(datatype, name, ListBuffer(arguments), body)

  // only one function argument and single statement body
  def apply(datatype : IR_Datatype, name : String, arguments : IR_FunctionArgument, body : IR_Statement) =
    new IR_Function(datatype, name, ListBuffer(arguments), ListBuffer(body))
}

case class IR_Function(
    var datatype : IR_Datatype,
    var name : String,
    var parameters : ListBuffer[IR_FunctionArgument],
    var body : ListBuffer[IR_Statement],
    var allowInlining : Boolean = true,
    var allowFortranInterface : Boolean = true,
    var functionQualifiers : String = "" // e.g. "__global__" etc
) extends IR_FunctionLike {

  override def prettyprint(out : PpStream) : Unit = {
    if (!functionQualifiers.isEmpty)
      out << functionQualifiers << ' '
    out << datatype << ' ' << name << ' ' << '(' <<< (parameters, ", ") << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }

  override def prettyprint_decl() : String = {
    var decl = ""
    if (!functionQualifiers.isEmpty)
      decl += functionQualifiers + ' '
    decl += datatype.prettyprint + ' ' + name + '(' + parameters.map(_.prettyprint()).mkString(", ") + ");\n"
    decl
  }
}
