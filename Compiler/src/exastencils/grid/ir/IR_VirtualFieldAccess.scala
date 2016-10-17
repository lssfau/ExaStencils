package exastencils.grid.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures._
import exastencils.grid.GridGeometry

/// IR_VirtualFieldAccess

case class IR_VirtualFieldAccess(var fieldName : String,
    var level : IR_Expression,
    var index : IR_ExpressionIndex,
    var arrayIndex : Option[Int] = None,
    var fragIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_Access with IR_SpecialExpandable {
  // TODO: extends IR_MultiDimFieldAccess
  // FIXME: datatype
  override def datatype = IR_RealDatatype
}

/// IR_ResolveVirtualFieldAccess

object IR_ResolveVirtualFieldAccess extends DefaultStrategy("Resolve virtual fields") {
  this += new Transformation("SearchAndReplace", {
    case virtualField : IR_VirtualFieldAccess => GridGeometry.getGeometry.invokeAccessResolve(virtualField)
  })
}
