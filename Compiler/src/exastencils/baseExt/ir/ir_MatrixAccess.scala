package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config._
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

/// IR_HackMatComponentAccess

// FIXME: update with actual accessors
case class IR_HackMatComponentAccess(var mat : IR_VariableAccess, var i : IR_Expression, var j : IR_Expression) extends IR_Expression {
  override def datatype = mat.innerDatatype.getOrElse(IR_RealDatatype)
  override def prettyprint(out : PpStream) : Unit = out << mat << "(" << i << ", " << j << ")"
}

/// IR_MatrixExpression

// FIXME: to be replaced/ updated
case class IR_MatrixExpression(var innerDatatype : Option[IR_Datatype], var expressions : ListBuffer[ListBuffer[IR_Expression]]) extends IR_Expression {
  override def datatype = {
    if (innerDatatype.isEmpty) {
      var l = expressions.flatten
      var ret = l(0).datatype
      l.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
      innerDatatype = Some(ret)
    }
    IR_MatrixDatatype(innerDatatype.getOrElse(IR_RealDatatype), this.rows, this.columns)
  }

  def prettyprintInner(out : PpStream) : Unit = {
    out << (if (Platform.targetCompiler == "GCC") "std::move((" else "((")
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out << "[]){" <<< (expressions.flatten, ",") << "})"
  }
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) "double" else "float"

    out << "Matrix<" << (if (isInteger) "int" else prec) << ", " << rows << ", " << columns << "> ("
    prettyprintInner(out)
    out << ")"
  }

  def rows = expressions.length
  def columns = expressions(0).length

  def apply(i : Integer) = expressions(i)
  def isConstant = expressions.flatten.forall(e => e.isInstanceOf[IR_Number])
  def isInteger = expressions.flatten.forall(e => e.isInstanceOf[IR_IntegerConstant])
}

