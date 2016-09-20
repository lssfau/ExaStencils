package exastencils.grid.l4

import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.l4.L4_ExpressionIndex
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.datastructures.l4._
import exastencils.grid.ir.IR_VirtualFieldAccess
import exastencils.knowledge
import exastencils.prettyprinting.PpStream

case class L4_VirtualFieldAccess(var name : String, var level : AccessLevelSpecification, var arrayIndex : Option[Int] = None, var offset : Option[L4_ExpressionIndex] = None) extends Access {
  def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (offset.isDefined) out << "@" << offset
  }

  def progress : IR_VirtualFieldAccess = {
    var numDims = knowledge.Knowledge.dimensionality // TODO: resolve field info
    if (arrayIndex.isDefined) numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = IR_LoopOverDimensions.defIt(numDims)
    if (arrayIndex.isDefined)
      multiIndex(numDims - 1) = IR_IntegerConstant(arrayIndex.get)
    if (offset.isDefined) {
      var progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= IR_IntegerConstant(0)
      multiIndex += progressedOffset
    }

    IR_VirtualFieldAccess(name, IR_IntegerConstant(level.asInstanceOf[SingleLevelSpecification].level), multiIndex, arrayIndex)
  }
}
