package exastencils.waLBerla.ir.blockforest

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_LoopOverNeighbors
import exastencils.baseExt.ir.IR_StdArrayDatatype
import exastencils.communication.DefaultNeighbors
import exastencils.config.Knowledge
import exastencils.domain.ir._
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember

/// IR_WaLBerlaBlockConnection
abstract class IR_WaLBerlaBlockConnection() extends IR_WaLBerlaInterfaceMember(true, false, true) {
  // exa fragment connection used as proxy
  protected def proxy : IR_IV_FragmentConnection

  def indexOfRefinedNeighbor : Option[IR_Expression]

  // exa IV indices
  def domainIdx : IR_Expression = IR_DomainCollection.getByIdentifier("global").get.index
  def neighIdx : IR_Expression
  def fragmentIdx : IR_Expression

  // waLBerla IV specifics extracted by wrapping proxy output
  override def isPrivate : Boolean = true
  override def name = "wb_" + proxy.resolveName()
  override def resolveDatatype() = {
    val baseDatatype = proxy.baseDatatype

    if (Knowledge.refinement_enabled) IR_StdArrayDatatype(baseDatatype, Knowledge.refinement_maxFineNeighborsForCommAxis) else baseDatatype
  }

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    val access = super.resolveAccess(baseAccess, fragment, domain, field, level, neigh)

    if (Knowledge.refinement_enabled) IR_ArrayAccess(access, if (indexOfRefinedNeighbor.isDefined) indexOfRefinedNeighbor.get else 0) else access
  }

  override def getCtor() : Option[IR_Statement] = {
    if (Knowledge.refinement_enabled && resolveDefValue().isDefined) {
      def resolveAccess(i : Int) : IR_Expression = {
        val access = super.resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_LoopOverNeighbors.defIt)

        IR_ArrayAccess(access, i)
      }

      Some(wrapInLoops(IR_Scope((0 until Knowledge.refinement_maxFineNeighborsForCommAxis).map(i => IR_Assignment(resolveAccess(i), resolveDefValue().get)) : _*)))
    } else {
      super.getCtor()
    }
  }

  override def resolveDefValue() = proxy.resolveDefValue()
  override def prettyprint(out : PpStream) = out << resolveAccess(resolveName(), fragmentIdx, domainIdx, IR_NullExpression, IR_NullExpression, neighIdx)
}

/// IR_WaLBerlaNeighborIsValid

case class IR_WaLBerlaNeighborIsValid(
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaBlockConnection with IR_IV_NeighborIsValidLike {

  override protected def proxy : IR_IV_FragmentConnection = IR_IV_NeighborIsValid(domainIdx, neighIdx, indexOfRefinedNeighbor, fragmentIdx)
}

/// IR_WaLBerlaNeighborIsRemote

case class IR_WaLBerlaNeighborIsRemote(
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaBlockConnection {

  override protected def proxy : IR_IV_FragmentConnection = IR_IV_NeighborIsRemote(domainIdx, neighIdx, indexOfRefinedNeighbor, fragmentIdx)
}

/// IR_WaLBerlaNeighborFragmentIdx

case class IR_WaLBerlaNeighborFragmentIdx(
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaBlockConnection {

  override protected def proxy : IR_IV_FragmentConnection = IR_IV_NeighborFragmentIdx(domainIdx, neighIdx, indexOfRefinedNeighbor, fragmentIdx)
}

/// IR_WaLBerlaNeighborRemoteRank

case class IR_WaLBerlaNeighborRemoteRank(
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaBlockConnection {

  override protected def proxy : IR_IV_FragmentConnection = IR_IV_NeighborRemoteRank(domainIdx, neighIdx, indexOfRefinedNeighbor, fragmentIdx)
}