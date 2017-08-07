package exastencils.boundary.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.communication._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir.IR_IV_NeighborIsValid

/// IR_IsOnBoundary

case class IR_IsOnBoundary(var field : IR_FieldSelection, var index : IR_ExpressionIndex) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype

  override def expand() : Output[IR_Expression] = {
    var applicableNeighbors = DefaultNeighbors.neighbors
    if (Knowledge.experimental_bc_checkOnlyMainAxis)
      applicableNeighbors = applicableNeighbors.filter(n => 1 == n.dir.map(d => math.abs(d)).sum)

    if (Knowledge.experimental_bc_avoidOrOperations)
      IR_Negation(applicableNeighbors.map(n => IR_Negation(IR_IsOnSpecBoundary(field, n, index).expand().inner) : IR_Expression).reduce((a, b) => IR_AndAnd(a, b)))
    else
      applicableNeighbors.map(n => IR_IsOnSpecBoundary(field, n, index).expand().inner).reduce((a, b) => IR_OrOr(a, b))
  }
}

/// IR_IsOnSpecBoundary

case class IR_IsOnSpecBoundary(var field : IR_FieldSelection, var neigh : NeighborInfo, var index : IR_ExpressionIndex) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype

  override def expand() : Output[IR_Expression] = {
    // should work for node, cell and face localizations

    var conditions = ListBuffer[IR_Expression](IR_Negation(IR_IV_NeighborIsValid(field.domainIndex, neigh.index)))
    for (dim <- 0 until field.field.fieldLayout.numDimsGrid) {
      neigh.dir(dim) match {
        case -1 => conditions += IR_Lower(Duplicate(index(dim)), field.fieldLayout.idxById("DLE", dim) - field.referenceOffset(dim))
        case 1  => conditions += IR_GreaterEqual(Duplicate(index(dim)), field.fieldLayout.idxById("DRB", dim) - field.referenceOffset(dim))
        case 0  => // true
      }
    }

    conditions.reduce((a, b) => IR_AndAnd(a, b))
  }
}

/// IR_IsValidComputationPoint

// checks for IR_IsOnBoundary as well as if outside inner/dup layers on fragment transitions
case class IR_IsValidComputationPoint(var field : IR_FieldSelection, var index : IR_ExpressionIndex) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype

  override def expand() : Output[IR_Expression] = {
    var applicableNeighbors = DefaultNeighbors.neighbors
    if (Knowledge.experimental_bc_checkOnlyMainAxis)
      applicableNeighbors = applicableNeighbors.filter(n => 1 == n.dir.map(d => math.abs(d)).sum)

    val isNotOnBoundary =
      if (Knowledge.experimental_bc_avoidOrOperations)
        applicableNeighbors.map(n => IR_Negation(IR_IsOnSpecBoundary(field, n, index).expand().inner) : IR_Expression).reduce((a, b) => IR_AndAnd(a, b))
      else
        IR_Negation(applicableNeighbors.map(n => IR_IsOnSpecBoundary(field, n, index).expand().inner).reduce((a, b) => IR_OrOr(a, b)))

    val isInnerOrDup =
      (0 until field.field.fieldLayout.numDimsGrid).map(dim =>
        IR_AndAnd(
          IR_Lower(Duplicate(index(dim)), field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim)),
          IR_GreaterEqual(Duplicate(index(dim)), field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim)))).reduce((a, b) => IR_AndAnd(a, b))

    IR_AndAnd(isNotOnBoundary, isInnerOrDup)
  }
}
