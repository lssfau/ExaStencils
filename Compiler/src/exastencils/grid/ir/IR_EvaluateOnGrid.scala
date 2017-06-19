package exastencils.grid.ir

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.grid._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// IR_EvaluateOnGrid

case class IR_EvaluateOnGrid(
    var name : String,
    var level : Int,
    var expression : IR_Expression,
    var interpolation : String,
    var offset : Option[IR_ConstIndex] = None) extends IR_Expression with IR_CanBeOffset {

  override def datatype : IR_Datatype = /* FIXME */ IR_UnknownDatatype
  override def prettyprint(out : PpStream) = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def offsetWith(newOffset : IR_ConstIndex) = {
    if (offset.isEmpty)
      offset = Some(newOffset)
    else
      offset = Some(offset.get + newOffset)
  }
}

/// IR_ResolveEvaluateOnGrid

object IR_ResolveEvaluateOnGrid extends DefaultStrategy("Resolve evaluation expressions") {
  this += new Transformation("Resolve", {
    case evaluate : IR_EvaluateOnGrid =>
      evaluate.expression match {
        case fieldAccess : IR_FieldAccess =>
          GridEvaluator.getEvaluator.invokeEvalResolve(evaluate.name, evaluate.level, fieldAccess, evaluate.interpolation, evaluate.offset)
        case _                            =>
          Logger.warn(s"Argument ${ evaluate.expression.prettyprint() } is currently not supported for function ${ evaluate.name }")
          evaluate.expression
      }
  })
}

/// IR_ExpandEvaluationExpressions

object IR_ExpandEvaluationExpressions extends DefaultStrategy("Expand evaluation expressions") {
  this += new Transformation("Expand", {
    case evaluate : GridEvaluator_AxisAligned.EvalAtRFace => evaluate.expandSpecial
  })
}
