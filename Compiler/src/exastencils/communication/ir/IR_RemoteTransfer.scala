package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.fieldlike.ir.IR_FieldLike

/// IR_RemoteTransfer

trait IR_RemoteTransfer extends  IR_Statement with IR_SpecialExpandable {
  def field : IR_FieldLike

  def expandSpecial() : ListBuffer[IR_Statement]
}

/// IR_ResolveRemoteTransfer

object IR_ResolveRemoteTransfer extends DefaultStrategy("Resolve nodes for remote data transfer") {
  this += Transformation("..", {
    case transfer : IR_RemoteTransfer => transfer.expandSpecial()
  })
}



