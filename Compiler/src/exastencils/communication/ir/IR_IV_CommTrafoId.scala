package exastencils.communication.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.domain.ir.IR_IV_FragmentConnection
import exastencils.prettyprinting.PpStream

/// IR_IV_CommTrafoId

case class IR_IV_CommTrafoId(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)
  override def resolveName() = s"commTrafoId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(false)
}

/// IR_IV_NeighFragId

case class IR_IV_NeighFragId(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)
  override def resolveName() = s"neighFragId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(false)
}

/// IR_IV_CommNeighIdx
// Index of the neighbor pointing back to the current fragment from the neighboring fragment
case class IR_IV_CommNeighNeighIdx(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)
  override def resolveName() = s"commNeighIdx" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(false)
}

