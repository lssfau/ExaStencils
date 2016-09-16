package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.prettyprinting._

@deprecated("should be removed completely, since it complicates AST analysis for transformations/optimization; please, don't use it in new code", "14.04.2016")
case class ConcatenationExpression(var expressions : ListBuffer[IR_Expression]) extends IR_Expression {
  def this(exprs : IR_Expression*) = this(exprs.to[ListBuffer])
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out <<< expressions
}

