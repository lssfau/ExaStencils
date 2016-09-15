package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.prettyprinting.PpStream
import exastencils.util.PrintStatement

/// IR_Define

object IR_Define {
  def apply(n : IR_Expression, v : IR_Expression) = new IR_Define(n, Option(v))
}

case class IR_Define(var name : IR_Expression, var value : Option[IR_Expression] = None) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "#define " << name
    if (value.isDefined)
      out << ' ' << value.get
  }
}

/// IR_Comment

case class IR_Comment(var comment : String) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "/* " << comment << " */"
}

/// IR_Assert

case class IR_Assert(var check : IR_Expression, var msg : ListBuffer[IR_Expression], var abort : IR_Statement) extends IR_Statement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = AssertStatement\n"
  override def expand = IR_IfCondition(IR_NegationExpression(check), ListBuffer(PrintStatement(msg), abort))
}

/// IR_InitializerList

object IR_InitializerList {
  def apply(args : IR_Expression*) = new IR_InitializerList(args.to[ListBuffer])
}

case class IR_InitializerList(var arguments : ListBuffer[IR_Expression]) extends IR_Expression {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "{ " <<< (arguments, ", ") << " }"
}
