package exastencils.domain.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.prettyprinting.PpStream

/// IR_IV_FragmentConnection
abstract class IR_IV_FragmentConnection extends IR_InternalVariable(true, true, false, false, true) {
  override def usesFragmentArrays : Boolean = true
  override def usesDomainArrays : Boolean = true
  override def usesNeighborArrays : Boolean = true
}

/// IR_IV_NeighborIsValid

case class IR_IV_NeighborIsValid(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName = s"neighbor_isValid" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype = IR_BooleanDatatype
  override def resolveDefValue = Some(false)
}

/// IR_IV_NeighborIsRemote

case class IR_IV_NeighborIsRemote(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName = s"neighbor_isRemote" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype = IR_BooleanDatatype
  override def resolveDefValue = Some(false)
}

/// IR_IV_NeighborFragmentIdx

case class IR_IV_NeighborFragmentIdx(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName = s"neighbor_fragCommId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype = IR_IntegerDatatype
  override def resolveDefValue = Some(-1)
}

/// IR_IV_NeighborRemoteRank

case class IR_IV_NeighborRemoteRank(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName = s"neighbor_remoteRank" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype = IR_IntegerDatatype
  override def resolveDefValue = Some("MPI_PROC_NULL")
}

