package exastencils.waLBerla.ir.blockforest

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.domain.ir._
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember

/// IR_WaLBerlaBlockConnection
abstract class IR_WaLBerlaBlockConnection() extends IR_WaLBerlaInterfaceMember(true, false, true) {
  // exa fragment connection used as proxy
  protected def proxy : IR_IV_FragmentConnection

  // exa IV indices
  def domainIdx : IR_Expression = IR_DomainCollection.getByIdentifier("global").get.index
  def neighIdx : IR_Expression
  def fragmentIdx : IR_Expression

  // waLBerla IV specifics extracted by wrapping proxy output
  override def isPrivate : Boolean = true
  override def name = "wb_" + proxy.resolveName()
  override def resolveDatatype() = proxy.resolveDatatype()
  override def resolveDefValue() = proxy.resolveDefValue()
  override def prettyprint(out : PpStream) = out << resolveAccess(resolveName(), fragmentIdx, domainIdx, IR_NullExpression, IR_NullExpression, neighIdx)
}

/// IR_WaLBerlaNeighborIsValid

case class IR_WaLBerlaNeighborIsValid(var neighIdx : IR_Expression, var indexOfRefinedNeighbor : Option[IR_Expression], var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaBlockConnection {
  override protected def proxy : IR_IV_FragmentConnection = IR_IV_NeighborIsValid(domainIdx, neighIdx, indexOfRefinedNeighbor, fragmentIdx)
}

/// IR_WaLBerlaNeighborIsRemote

case class IR_WaLBerlaNeighborIsRemote(var neighIdx : IR_Expression, var indexOfRefinedNeighbor : Option[IR_Expression], var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaBlockConnection {
  override protected def proxy : IR_IV_FragmentConnection = IR_IV_NeighborIsRemote(domainIdx, neighIdx, indexOfRefinedNeighbor, fragmentIdx)
}

/// IR_WaLBerlaNeighborFragmentIdx

case class IR_WaLBerlaNeighborFragmentIdx(var neighIdx : IR_Expression, var indexOfRefinedNeighbor : Option[IR_Expression], var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaBlockConnection {
  override protected def proxy : IR_IV_FragmentConnection = IR_IV_NeighborFragmentIdx(domainIdx, neighIdx, indexOfRefinedNeighbor, fragmentIdx)
}

/// IR_WaLBerlaNeighborRemoteRank

case class IR_WaLBerlaNeighborRemoteRank(var neighIdx : IR_Expression, var indexOfRefinedNeighbor : Option[IR_Expression], var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaBlockConnection {
  override protected def proxy : IR_IV_FragmentConnection = IR_IV_NeighborRemoteRank(domainIdx, neighIdx, indexOfRefinedNeighbor, fragmentIdx)
}