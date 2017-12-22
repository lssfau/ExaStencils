package exastencils.grid.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.baseExt.l2._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.grid.l3._
import exastencils.knowledge.l2.L2_LeveledKnowledgeAccess
import exastencils.optimization.l2.L2_GeneralSimplifyWrapper
import exastencils.prettyprinting.PpStream

/// L2_VirtualFieldAccess

object L2_VirtualFieldAccess {
  def apply(access : L2_FutureVirtualFieldAccess) = {
    val target = L2_VirtualFieldCollection.getByIdentifier(access.name, access.level).get
    val offset = access.offset

    var index = L2_FieldIteratorAccess.fullIndex(target.numDims)
    if (offset.isDefined) index += offset.get

    new L2_VirtualFieldAccess(target, index)
  }
}

case class L2_VirtualFieldAccess(
    var target : L2_VirtualField,
    var index : L2_ExpressionIndex) extends L2_LeveledKnowledgeAccess with L2_CanBeOffset with L2_MayBlockResolution {

  allDone = !(target.resolutionPossible && Knowledge.experimental_l2_resolveVirtualFields)

  override def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level << '@' << extractOffset
  }

  override def offsetWith(offset : L2_ConstIndex) = index += offset

  def extractOffset = {
    var offset = Duplicate(index) - L2_FieldIteratorAccess.fullIndex(target.numDims)
    offset = L2_GeneralSimplifyWrapper.process(offset)
    offset.toConstIndex
  }

  def tryResolve : L2_Expression = {
    if (!target.resolutionPossible)
      return this // do nothing

    target match {
      case scalar : L2_VirtualFieldWithScalar => scalar.resolve(index)
      case vector : L2_VirtualFieldWithVec    => this // FIXME: L2_VectorExpression(vector.listPerDim.zipWithIndex.map((va, dim) => L2_VirtualFieldAccess(va, Duplicate(offset), dim)))
    }
  }

  override def progress = ProgressLocation(L3_VirtualFieldAccess(target.getProgressedObj(), index.progress))
}

/// L2_ResolveVirtualFieldAccesses

object L2_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L2_FutureVirtualFieldAccess if L2_VirtualFieldCollection.exists(access.name, access.level) =>
      access.toVirtualFieldAccess

    // attempt further resolution if requested
    case access : L2_VirtualFieldAccess if !access.allDone =>
      access.tryResolve
  })
}
