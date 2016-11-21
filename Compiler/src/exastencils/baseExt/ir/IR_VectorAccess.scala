package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Platform
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

/// IR_HackVecComponentAccess

// FIXME: update with actual accessors
case class IR_HackVecComponentAccess(var vec : IR_VariableAccess, var i : IR_Expression) extends IR_Expression {
  override def datatype = vec.datatype
  override def prettyprint(out : PpStream) : Unit = out << vec << "(" << i << ", " << 0 << ")"
}

/// IR_VectorExpression

// FIXME: to be replaced/ updated
case class IR_VectorExpression(var innerDatatype : Option[IR_Datatype], var expressions : ListBuffer[IR_Expression], var rowVector : Option[Boolean]) extends IR_Expression {
  def length = expressions.length

  def apply(i : Integer) = expressions(i)
  def isConstant = expressions.forall(e => e.isInstanceOf[IR_Number])

  override def datatype = {
    if (innerDatatype.isEmpty) {
      var ret = expressions(0).datatype
      expressions.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
      innerDatatype = Some(ret)
    }
    IR_VectorDatatype(innerDatatype.getOrElse(IR_RealDatatype), expressions.length, rowVector)
  }
  def prettyprintInner(out : PpStream) : Unit = {
    out << (if (Platform.targetCompiler == "GCC") "std::move((" else "((")
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out << "[]){" <<< (expressions, ",") << "})"
  }
  override def prettyprint(out : PpStream) : Unit = {
    out << "Matrix<"
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out << ", "
    if (rowVector.getOrElse(true)) {
      out << "1, " << length << "> (" // row vector
    } else {
      out << length << ", 1> ("
    }
    prettyprintInner(out)
    out << ')'
  }
}

