package exastencils.domain.ir

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.config.Knowledge

case class IR_IV_Nfragments() extends IR_InternalVariable(false, false, false, false, false) {
  override def resolveName() = s"nFragments" + resolvePostfix("", "", "", "", "")
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(Knowledge.domain_numFragmentsPerBlock)
}
