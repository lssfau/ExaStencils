package exastencils.grid.l3

import exastencils.base.l3._
import exastencils.baseExt.l3._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.grid.l4._
import exastencils.knowledge.l3.L3_LeveledKnowledgeAccess
import exastencils.optimization.l3.L3_GeneralSimplifyWrapper
import exastencils.prettyprinting.PpStream

/// L3_VirtualFieldAccess

object L3_VirtualFieldAccess {
  def apply(access : L3_FutureVirtualFieldAccess) = {
    val target = L3_VirtualFieldCollection.getByIdentifier(access.name, access.level).get
    val offset = access.offset

    var index = L3_FieldIteratorAccess.fullIndex(target.numDims)
    if (offset.isDefined) index += offset.get

    new L3_VirtualFieldAccess(target, index)
  }
}

case class L3_VirtualFieldAccess(
    var target : L3_VirtualField,
    var index : L3_ExpressionIndex) extends L3_LeveledKnowledgeAccess with L3_CanBeOffset {

  def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level << '@' << extractOffset
  }

  override def offsetWith(offset : L3_ConstIndex) = index += offset

  def extractOffset = {
    var offset = Duplicate(index) - L3_FieldIteratorAccess.fullIndex(target.numDims)
    offset = L3_GeneralSimplifyWrapper.process(offset)
    offset.toConstIndex
  }

  def tryResolve : L3_Expression = {
    if (!target.resolutionPossible)
      return this // do nothing

    target match {
      case scalar : L3_VirtualFieldWithScalar => scalar.resolve(index)
      case vector : L3_VirtualFieldWithVec    => this // FIXME: L3_VectorExpression(vector.listPerDim.zipWithIndex.map((va, dim) => L3_VirtualFieldAccess(va, Duplicate(offset), dim)))
    }
  }

  def progress = L4_VirtualFieldAccess(target.getProgressedObj(), index.progress)
}

/// L3_ResolveVirtualFieldAccesses

object L3_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureVirtualFieldAccess if L3_VirtualFieldCollection.exists(access.name, access.level) =>
      def newAccess = access.toVirtualFieldAccess
      // attempt further resolution if requested
      if (Knowledge.experimental_l3_resolveVirtualFields)
        newAccess.tryResolve
      else
        newAccess
  })
}
