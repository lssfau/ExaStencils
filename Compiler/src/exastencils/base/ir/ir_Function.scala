package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.core.Duplicate
import exastencils.prettyprinting._

abstract class IR_AbstractFunction(var isHeaderOnly : Boolean = false) extends IR_Statement {
  def name : String
  def prettyprint_decl() : String
}

/// IR_FunctionArgument

case class IR_FunctionArgument(var name : String, var datatype : IR_Datatype) extends IR_Node with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << datatype << ' ' << name

  def access = IR_VariableAccess(name, Duplicate(datatype))
}

/// IR_Function

object IR_Function {
  // no function arguments
  def apply(returntype : IR_Datatype, name : String, body : ListBuffer[IR_Statement]) =
  new IR_Function(returntype, name, ListBuffer(), body)

  // single statement body
  def apply(returntype : IR_Datatype, name : String, arguments : ListBuffer[IR_FunctionArgument], body : IR_Statement) =
  new IR_Function(returntype, name, arguments, ListBuffer(body))

  // only one function argument
  def apply(returntype : IR_Datatype, name : String, arguments : IR_FunctionArgument, body : ListBuffer[IR_Statement]) =
  new IR_Function(returntype, name, ListBuffer(arguments), body)

  // only one function argument and single statement body
  def apply(returntype : IR_Datatype, name : String, arguments : IR_FunctionArgument, body : IR_Statement) =
  new IR_Function(returntype, name, ListBuffer(arguments), ListBuffer(body))
}

case class IR_Function(
    var returntype : IR_Datatype,
    var name : String,
    var parameters : ListBuffer[IR_FunctionArgument],
    var body : ListBuffer[IR_Statement],
    var allowInlining : Boolean = true,
    var allowFortranInterface : Boolean = true,
    var functionQualifiers : String = "" // e.g. "__global__" etc
) extends IR_AbstractFunction {

  override def prettyprint(out : PpStream) : Unit = {
    if (!functionQualifiers.isEmpty)
      out << functionQualifiers << ' '
    out << returntype << ' ' << name << ' ' << '(' <<< (parameters, ", ") << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }

  override def prettyprint_decl() : String = {
    var decl = ""
    if (!functionQualifiers.isEmpty)
      decl += functionQualifiers + ' '
    decl += returntype.prettyprint + ' ' + name + '(' + parameters.map(_.prettyprint()).mkString(", ") + ");\n"
    decl
  }
}

/// IR_Return

object IR_Return {
  def apply(expr : IR_Expression) = new IR_Return(Option(expr))
}

case class IR_Return(var expr : Option[IR_Expression] = None) extends IR_Statement {
  override def prettyprint(out : PpStream) = {
    out << "return"
    if (expr.isDefined)
      out << ' ' << expr.get.prettyprint()
    out << ';'
  }
}