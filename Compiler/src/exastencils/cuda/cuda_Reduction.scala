package exastencils.cuda

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_Linearization
import exastencils.datastructures._
import exastencils.datastructures.ir.iv
import exastencils.prettyprinting.PpStream

/// IR_ReductionDeviceDataAccess

case class IR_ReductionDeviceDataAccess(var data : iv.ReductionDeviceData, var index : IR_ExpressionIndex, var strides : IR_ExpressionIndex) extends IR_Expression {
  override def datatype = data.datatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def linearize = IR_ArrayAccess(data, IR_Linearization.linearizeIndex(index, strides), alignedAccessPossible = false)
}

/// IR_LinearizeReductionDeviceDataAccess

object IR_LinearizeReductionDeviceDataAccess extends DefaultStrategy("Linearize ReductionDeviceDataAccess nodes") {
  this += new Transformation("Linearize", {
    case access : IR_ReductionDeviceDataAccess => access.linearize
  })
}
