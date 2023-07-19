package exastencils.domain.ir

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