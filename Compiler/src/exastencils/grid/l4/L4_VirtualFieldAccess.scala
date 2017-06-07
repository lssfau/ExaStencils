package exastencils.grid.l4

import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.grid.ir.IR_VirtualFieldAccess
import exastencils.knowledge.l4._
import exastencils.prettyprinting.PpStream

/// L4_VirtualFieldAccess

object L4_VirtualFieldAccess {
  def apply(access : L4_FutureVirtualFieldAccess) =
    new L4_VirtualFieldAccess(L4_VirtualFieldCollection.getByIdentifier(access.name, access.level).get, access.offset, access.arrayIndex)
}

case class L4_VirtualFieldAccess(
    var target : L4_VirtualField,
    var offset : Option[L4_ConstIndex] = None,
    var arrayIndex : Option[Int] = None) extends L4_LeveledKnowledgeAccess {

  def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << "@" << offset.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
  }

  def progress : IR_VirtualFieldAccess = {
    var numDims = Knowledge.dimensionality // TODO: resolve field info
    if (arrayIndex.isDefined) numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = IR_LoopOverDimensions.defIt(numDims)
    if (arrayIndex.isDefined)
      multiIndex(numDims - 1) = IR_IntegerConstant(arrayIndex.get)
    if (offset.isDefined) {
      var progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= 0
      multiIndex += progressedOffset
    }

    IR_VirtualFieldAccess(name, level, multiIndex, arrayIndex)
  }
}

/// L4_ResolveVirtualFieldAccesses

object L4_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L4_FutureVirtualFieldAccess if L4_VirtualFieldCollection.exists(access.name, access.level) =>
      access.toVirtualFieldAccess
  })
}
