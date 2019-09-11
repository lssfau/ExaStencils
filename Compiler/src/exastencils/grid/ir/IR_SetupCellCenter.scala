package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.logger.Logger

/// IR_SetupCellCenter

object IR_SetupCellCenter {
  def for_nonAA(level : Int) : ListBuffer[IR_Statement] = {
    if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to global domain")

    val domain = IR_DomainCollection.getByIdentifier("global").get
    val numDims = domain.numDims

    val field = IR_VF_CellCenterAsVec.find(level).associatedField
    val baseIndex = IR_LoopOverDimensions.defIt(numDims)
    val baseAccess = IR_FieldAccess(field, 0, baseIndex)

    val npField = IR_VF_NodePositionAsVec.find(level).associatedField
    var interpolateExps = ListBuffer(IR_FieldAccess(npField, 0, baseIndex))
    var factor = 1.0
    for (dim <- Knowledge.dimensions) {
      interpolateExps = interpolateExps.flatMap(fieldAccess =>
        ListBuffer(Duplicate(fieldAccess), IR_GridUtil.offsetAccess(fieldAccess, 1, dim)))
      factor /= 2.0
    }

    ListBuffer[IR_Statement](
      IR_LoopOverPoints(field, None,
        IR_ExpressionIndex(Array.fill(numDims)(-1)),
        IR_ExpressionIndex(Array.fill(numDims)(-1)),
        IR_ExpressionIndex(1, 1, 1),
        ListBuffer[IR_Statement](IR_Assignment(Duplicate(baseAccess), factor * IR_Addition(interpolateExps.map(e => e : IR_Expression))))))
  }
}
