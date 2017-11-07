package exastencils.operator.l1

import exastencils.base.l1.L1_Expression
import exastencils.grid.l1.L1_Localization
import exastencils.logger.Logger
import exastencils.parsers.l1.L1_ReservedSigns
import exastencils.prettyprinting.PpStream

/// L1_PartialDerivative

object L1_PartialDerivative {
  def apply(stringRep : String) = {
    val cases = stringRep.toCharArray.distinct.sorted
    if (cases.length > 1) Logger.error("Mixed partial derivatives are currently not supported")

    val dimAsChar = cases.head
    new L1_PartialDerivative(stringRep.count(_ == dimAsChar), L1_Localization.stringToDim(dimAsChar))
  }
}

case class L1_PartialDerivative(order : Int, dim : Int) extends L1_Expression {
  override def prettyprint(out : PpStream) = out << L1_ReservedSigns.partial._2 << "_{" << L1_Localization.dimToString(dim).toString * order << "}"
  override def progress = Logger.error("Trying to progress L1 partial derivative; unsupported")
}
