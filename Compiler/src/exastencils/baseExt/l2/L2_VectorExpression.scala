package exastencils.baseExt.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.baseExt.l3.L3_VectorExpression
import exastencils.prettyprinting.PpStream

/// L2_VectorExpression

case class L2_VectorExpression(var entries : ListBuffer[L2_Expression], var rowVector : Boolean = false) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = ???
  override def progress = ProgressLocation(L3_VectorExpression(entries.map(_.progress), rowVector))
}
