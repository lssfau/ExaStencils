package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures.Transformation._
import exastencils.knowledge._
import exastencils.prettyprinting._
import exastencils.util._

case class VariableDeclarationStatement(var datatype : IR_Datatype, var name : String, var expression : Option[IR_Expression] = None) extends IR_Statement {
  var alignment : Int = 1
  def this(dT : IR_Datatype, n : String, e : IR_Expression) = this(dT, n, Option(e))
  def this(va : IR_VariableAccess) = this(va.innerDatatype.get, va.name, None)
  def this(va : IR_VariableAccess, e : IR_Expression) = this(va.innerDatatype.get, va.name, Option(e))

  override def prettyprint(out : PpStream) : Unit = {
    datatype match {
      case x : IR_VectorDatatype => {
        out << x << ' ' << name
        if (expression.isDefined) {
          out << "("
          expression.get.asInstanceOf[VectorExpression].prettyprintInner(out)
          out << ")"
        }
      }
      case x : IR_MatrixDatatype => {
        out << x << ' ' << name
        if (expression.isDefined) {
          out << "("
          expression.get.asInstanceOf[MatrixExpression].prettyprintInner(out)
          out << ")"
        }
      }
      case _                     => {
        if (alignment > 1 && "MSVC" == Platform.targetCompiler)
          out << "__declspec(align(" << alignment * 8 << ")) "
        out << datatype.resolveDeclType << ' ' << name << datatype.resolveDeclPostscript
        if (alignment > 1 && "MSVC" != Platform.targetCompiler)
          out << " __attribute__((aligned(" << alignment * 8 << ")))"
        if (expression.isDefined)
          out << " = " << expression.get
      }
    }

    out << ';'
  }

  def prettyprint_onlyDeclaration() : String = VariableDeclarationStatement(datatype, name, None).prettyprint()
}

case class ObjectInstantiation(var datatype : IR_Datatype, var name : String, var ctorArgs : ListBuffer[IR_Expression]) extends IR_Statement {
  def this(datatype : IR_Datatype, name : String, ctorArgs : IR_Expression*) = this(datatype, name, ctorArgs.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    out << datatype.resolveDeclType << ' ' << name << datatype.resolveDeclPostscript
    if (ctorArgs.length > 0)
      out << '(' <<< (ctorArgs, ", ") << ')'
    out << ';'
  }
}

case class DefineStatement(var name : IR_Expression, var value : Option[IR_Expression] = None) extends IR_Statement {
  def this(n : IR_Expression, v : IR_Expression) = this(n, Option(v))

  override def prettyprint(out : PpStream) : Unit = {
    out << "#define " << name
    if (value.isDefined)
      out << ' ' << value.get
  }
}

case class CommentStatement(var comment : String) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "/* " << comment << " */"
}

case class CaseStatement(var toMatch : IR_Expression, var body : ListBuffer[IR_Statement]) extends IR_Statement {
  def this(toMatch : IR_Expression, body : IR_Statement*) = this(toMatch, body.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    out << "case " << toMatch << ": {\n"
    out <<< (body, "\n") << '\n'
    out << "} break;"
  }
}

case class SwitchStatement(var what : IR_Expression, var body : ListBuffer[CaseStatement]) extends IR_Statement {
  def this(what : IR_Expression, body : CaseStatement*) = this(what, body.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    out << "switch (" << what << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

case class BreakStatement() extends IR_Statement {
  override def prettyprint(out : PpStream) = {
    out << "break;"
  }
}

case class AssertStatement(var check : IR_Expression, var msg : ListBuffer[IR_Expression], var abort : IR_Statement) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = AssertStatement\n"

  override def expand : Output[IR_IfCondition] = {
    IR_IfCondition(IR_NegationExpression(check),
      ListBuffer[IR_Statement](new PrintStatement(msg), abort))
  }
}
