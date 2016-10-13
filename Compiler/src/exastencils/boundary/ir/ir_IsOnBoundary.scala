package exastencils.boundary.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.communication._
import exastencils.config._
import exastencils.datastructures.Transformation.Output
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir.IR_IV_NeighborIsValid
import exastencils.prettyprinting.PpStream

/// IR_IsOnBoundary

case class IR_IsOnBoundary(var field : IR_FieldSelection, var index : IR_ExpressionIndex) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_Expression] = {
    var applicableNeighbors = DefaultNeighbors.neighbors
    if (Knowledge.experimental_bc_checkOnlyMainAxis)
      applicableNeighbors = applicableNeighbors.filter(n => 1 == n.dir.map(d => math.abs(d)).sum)

    if (Knowledge.experimental_bc_avoidOrOperations)
      IR_NegationExpression(applicableNeighbors.map(n => IR_NegationExpression(IR_IsOnSpecBoundary(field, n, index).expand().inner) : IR_Expression).reduce((a, b) => IR_AndAndExpression(a, b)))
    else
      applicableNeighbors.map(n => IR_IsOnSpecBoundary(field, n, index).expand().inner).reduce((a, b) => IR_OrOrExpression(a, b))
  }
}

/// IR_IsOnSpecBoundary

case class IR_IsOnSpecBoundary(var field : IR_FieldSelection, var neigh : NeighborInfo, var index : IR_ExpressionIndex) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_Expression] = {
    // should work for node, cell and face discretizations

    var conditions = ListBuffer[IR_Expression](IR_NegationExpression(IR_IV_NeighborIsValid(field.domainIndex, neigh.index)))
    for (dim <- 0 until field.field.fieldLayout.numDimsGrid) {
      neigh.dir(dim) match {
        case -1 => conditions += IR_LowerExpression(index(dim), field.fieldLayout.idxById("DLE", dim) - field.referenceOffset(dim))
        case 1  => conditions += IR_GreaterEqualExpression(index(dim), field.fieldLayout.idxById("DRB", dim) - field.referenceOffset(dim))
        case 0  => // true
      }
    }

    conditions.reduce((a, b) => IR_AndAndExpression(a, b))
  }
}

/// IR_IsValidComputationPoint

// checks for IR_IsOnBoundary as well as if outside inner/dup layers on fragment transitions
case class IR_IsValidComputationPoint(var field : IR_FieldSelection, var index : IR_ExpressionIndex) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_Expression] = {
    var applicableNeighbors = DefaultNeighbors.neighbors
    if (Knowledge.experimental_bc_checkOnlyMainAxis)
      applicableNeighbors = applicableNeighbors.filter(n => 1 == n.dir.map(d => math.abs(d)).sum)

    val isNotOnBoundary =
      if (Knowledge.experimental_bc_avoidOrOperations)
        applicableNeighbors.map(n => IR_NegationExpression(IR_IsOnSpecBoundary(field, n, index).expand().inner) : IR_Expression).reduce((a, b) => IR_AndAndExpression(a, b))
      else
        IR_NegationExpression(applicableNeighbors.map(n => IR_IsOnSpecBoundary(field, n, index).expand().inner).reduce((a, b) => IR_OrOrExpression(a, b)))

    val isInnerOrDup =
      (0 until field.field.fieldLayout.numDimsGrid).map(dim =>
        IR_AndAndExpression(
          IR_LowerExpression(index(dim), field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim)),
          IR_GreaterEqualExpression(index(dim), field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim)))).reduce((a, b) => IR_AndAndExpression(a, b))

    IR_AndAndExpression(isNotOnBoundary, isInnerOrDup)
  }
}
