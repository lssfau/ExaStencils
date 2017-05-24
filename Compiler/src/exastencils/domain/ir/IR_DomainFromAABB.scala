package exastencils.domain.ir

import exastencils.util.ir.IR_AABB

/// IR_DomainFromAABB

case class IR_DomainFromAABB(var name : String, aabb : IR_AABB) extends IR_Domain {
  var index = IR_Domain.runningIndex
  IR_Domain.runningIndex += 1

  override def HACK_shape : Any = ???
}
