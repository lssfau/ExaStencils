package exastencils.base.l1

import exastencils.prettyprinting._

case class L1_Interval(begin : Double, end : Double) extends L1_Node with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << "(" << begin << ", " << end << ")"
}