package exastencils.grid.l4

import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.grid.VirtualField
import exastencils.grid.ir.IR_VirtualFieldAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

case class L4_VirtualFieldAccess(
    var name : String,
    var level : L4_AccessLevelSpecification,
    var arrayIndex : Option[Int] = None,
    var offset : Option[L4_ExpressionIndex] = None) extends L4_Access {

  def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (offset.isDefined) out << "@" << offset
  }

  def progress : IR_VirtualFieldAccess = {
    var numDims = Knowledge.dimensionality // TODO: resolve field info
    if (arrayIndex.isDefined) numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = IR_LoopOverDimensions.defIt(numDims)
    if (arrayIndex.isDefined)
      multiIndex(numDims - 1) = IR_IntegerConstant(arrayIndex.get)
    if (offset.isDefined) {
      var progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= IR_IntegerConstant(0)
      multiIndex += progressedOffset
    }

    IR_VirtualFieldAccess(name, IR_IntegerConstant(level.resolveLevel), multiIndex, arrayIndex)
  }
}

/// L4_ResolveVirtualFieldAccesses

object L4_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if VirtualField.fields.contains(access.name.toLowerCase()) =>
      // resolveToVirtualFieldAccess
      if (access.dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on special field - was an offset access (@) intended?")
      if (access.slot.isDefined) Logger.warn("Discarding meaningless slot access on special field")
      L4_VirtualFieldAccess(access.name, access.level.get, access.arrayIndex, access.offset)
  })
}
