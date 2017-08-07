package exastencils.grid.ir

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.grid.GridEvaluator
import exastencils.prettyprinting.PpStream

/// IR_IntegrateOnGrid

case class IR_IntegrateOnGrid(
    var name : String,
    var level : Int,
    var expression : IR_Expression,
    var offset : Option[IR_ConstIndex]) extends IR_Expression with IR_CanBeOffset {

  override def datatype : IR_Datatype = /* FIXME */ IR_UnknownDatatype
  override def prettyprint(out : PpStream) = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def offsetWith(newOffset : IR_ConstIndex) = {
    if (offset.isEmpty)
      offset = Some(newOffset)
    else
      offset = Some(offset.get + newOffset)
  }
}

/// IR_ResolveIntegrateOnGrid

object IR_ResolveIntegrateOnGrid extends DefaultStrategy("Resolve integration expressions") {
  this += new Transformation("Resolve", {
    case integrate : IR_IntegrateOnGrid =>
      GridEvaluator.getEvaluator.invokeIntegrateResolve(integrate.name, integrate.level, integrate.expression, integrate.offset)
  })
}
