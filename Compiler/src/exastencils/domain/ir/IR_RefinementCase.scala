package exastencils.domain.ir

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.prettyprinting.PpStream

/// RefinementCase

object RefinementCase extends Enumeration {
  type Access = Value
  final val ANNOT : String = "RefinementCase"

  // EQUAL: send message to one neighbor (per fragment, per commAxis)
  // F2C  : send message to one (coarse) neighbor (per fragment, per commAxis)
  // C2F  : send messages to N (fine) neighbors (per fragment, per commAxis)
  final val EQUAL, F2C, C2F = Value

  def getOppositeCase(access: Access) = access match {
    case EQUAL => EQUAL
    case F2C => C2F
    case C2F => F2C
  }
}

/// IR_IV_NeighborRefinementCase

case class IR_IV_NeighborRefinementCase(
    var fragmentIdx : IR_Expression,
    var domain : IR_Expression,
    var neighIdx : IR_Expression) extends IR_InternalVariable(true, true, false, false, true) {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)
  override def resolveName() = s"refinementCase_" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(0)
}