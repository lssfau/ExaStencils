package exastencils.base.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.l4._
import exastencils.prettyprinting._

/// L4_FunctionArgument

case class L4_FunctionArgument(var name : String, var datatype : L4_Datatype) extends L4_Node with PrettyPrintable with L4_Progressable {
  override def prettyprint(out : PpStream) = out << name << " : " << datatype
  override def progress = IR_FunctionArgument(name, datatype.progress)
}

/// L4_Function

object L4_Function {
  def apply(identifier : Identifier, returntype : L4_Datatype, arguments : List[L4_FunctionArgument], statements : List[L4_Statement], allowInlining : Boolean) =
    new L4_Function(identifier, returntype, arguments.to[ListBuffer], statements.to[ListBuffer], allowInlining)
}

case class L4_Function(
    override var identifier : Identifier,
    var returntype : L4_Datatype,
    var arguments : ListBuffer[L4_FunctionArgument],
    var statements : ListBuffer[L4_Statement],
    var allowInlining : Boolean = true) extends L4_Statement with HasIdentifier {

  override def prettyprint(out : PpStream) = {
    out << "Function " << identifier << " (" <<< (arguments, ", ") << " )" << " : " << returntype << " {\n"
    out <<< statements
    out << "}\n"
  }

  override def progress = IR_Function(returntype.progress, identifier.fullName, arguments.map(s => s.progress), statements.map(s => s.progress), allowInlining)
}

/// L4_FunctionCall

object L4_FunctionCall {
  def apply(identifier : Access, arguments : L4_Expression*) = new L4_FunctionCall(identifier, arguments.to[ListBuffer])
}

case class L4_FunctionCall(var identifier : Access, var arguments : ListBuffer[L4_Expression]) extends L4_Expression {
  def prettyprint(out : PpStream) = out << identifier << " ( " <<< (arguments, ", ") << " )"
  def progress = IR_FunctionCall(identifier.progress.asInstanceOf[IR_StringLiteral].value, arguments.map(s => s.progress))
}

/// L4_Return

case class L4_Return(var expr : Option[L4_Expression]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "return"
    if (expr.isDefined) out << ' ' << expr.get.prettyprint()
    out << '\n'
  }

  override def progress = IR_Return(L4_ProgressOption(expr)(_.progress))
}
