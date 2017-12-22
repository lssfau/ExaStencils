package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.prettyprinting._

/// L4_ContractionSpecification

/// TODO: inline L4/IR_ContractionSpecification into respective ContractingLoop node?
case class L4_ContractionSpecification(var posExt : L4_ConstIndex, var negExt : Option[L4_ConstIndex]) extends L4_Node with L4_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = {
    out << posExt
    if (negExt.isDefined) out << ", " << negExt
  }

  override def progress = ProgressLocation(IR_ContractionSpecification(posExt.progress, negExt.getOrElse(posExt).progress))
}

/// L4_ContractingLoop

object L4_ContractingLoop {
  def apply(number : Int, iterator : Option[L4_Access], contraction : L4_ContractionSpecification, body : List[L4_Statement]) =
    new L4_ContractingLoop(number, iterator, contraction, body.to[ListBuffer])
}

case class L4_ContractingLoop(
    var number : Int,
    var iterator : Option[L4_Access],
    var contraction : L4_ContractionSpecification,
    var body : ListBuffer[L4_Statement]) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    out << "repeat " << number << " times"
    if (iterator.isDefined) out << " count " << iterator.get
    out << " with contraction " << contraction
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = ProgressLocation(IR_ContractingLoop(number, L4_ProgressOption(iterator)(_.progress), body.map(_.progress), contraction.progress))
}
