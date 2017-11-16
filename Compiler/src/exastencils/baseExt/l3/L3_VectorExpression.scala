package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.baseExt.l4.L4_VectorExpression
import exastencils.prettyprinting.PpStream

/// L3_VectorExpression

case class L3_VectorExpression(var entries : ListBuffer[L3_Expression], var rowVector : Boolean = false) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = ???
  override def progress = L4_VectorExpression(None, entries.map(_.progress).toList, rowVector)
}
