package exastencils.parallelization.api.cuda

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir._
import exastencils.config._
import exastencils.field.ir._
import exastencils.prettyprinting._

/// CUDA_HostDataUpdated

// TODO: move to communication package?
case class CUDA_HostDataUpdated(override var field : IR_Field, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FieldFlag {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index, field.level, IR_NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveName() = s"hostDataUpdated" + resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, field.level.toString, "")
  override def resolveDefValue() = Some(true)
}

/// CUDA_DeviceDataUpdated

// TODO: move to communication package?
case class CUDA_DeviceDataUpdated(override var field : IR_Field, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FieldFlag {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index, field.level, IR_NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveName() = s"deviceDataUpdated" + resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, field.level.toString, "")
  override def resolveDefValue() = Some(false)
}

/// CUDA_HostBufferDataUpdated

// TODO: move to communication package?
case class CUDA_HostBufferDataUpdated(var field : IR_Field, var direction : String, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName() = s"hostBufferDataUpdated_$direction" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype() = IR_BooleanDatatype
  override def resolveDefValue() = Some(false)
}

/// CUDA_DeviceBufferDataUpdated

// TODO: move to communication package?
case class CUDA_DeviceBufferDataUpdated(var field : IR_Field, var direction : String, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName() = s"deviceBufferDataUpdated_$direction" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype() = IR_BooleanDatatype
  override def resolveDefValue() = Some(false)
}
