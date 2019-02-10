package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.communication.DefaultNeighbors
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.field.ir.IR_DirectFieldAccess
import exastencils.field.ir.IR_FieldCollection
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression

case class IR_CommTransformation(var dim: Int, var trafoId: Int){
  // 0 --> W
  // 1 --> E
  // 2 --> S
  // 3 --> N

  def transformIndex(index: Array[IR_Expression], indexSize : Array[Long], indexBegin : Array[Long]) : Array[IR_Expression] = {
    trafoId match{
      case 0 => index
      case 1 =>
        Array[IR_Expression](
          index(0),
          indexSize(1) - index(1) + 2 * indexBegin(1)
        )
      case 2 =>
        Array[IR_Expression](
          indexSize(0) - index(0) + 2 * indexBegin(0),
          indexSize(1) - index(1) + 2 * indexBegin(1)
        )
      case 3 =>
        Array[IR_Expression](
          indexSize(0) - index(0) + 2 * indexBegin(0),
          index(1)
        )
    }
  }

  def switchUL(fieldName : String) = {
    fieldName.dropRight(1) + (fieldName.last match {
      case 'U' => 'L'
      case 'L' => 'U'
      case _ =>
        fieldName.last
    })
  }


  def applyTrafo(fieldAccess: IR_DirectFieldAccess, thisIndexRange : IR_ExpressionIndexRange, neigh : NeighborInfo) = {
    var indexSize = (thisIndexRange.end - thisIndexRange.begin).indices.map(IR_SimplifyExpression.evalIntegral).map(_ - 1)
    var indexBegin = thisIndexRange.begin.indices.map(IR_SimplifyExpression.evalIntegral)

    val index = fieldAccess.index

    val trafoIndex = IR_ExpressionIndex(transformIndex(index.indices, indexSize, indexBegin)
      ++ index.drop(fieldAccess.fieldSelection.field.numDimsGrid)
    )

    val transformedFieldAccess = IR_DirectFieldAccess(fieldAccess.fieldSelection, trafoIndex)

    trafoId match {
        // TODO check that U/L-switch must be done in these cases
      case 2 | 3 => {
        val newName = switchUL(transformedFieldAccess.fieldSelection.field.name)
        transformedFieldAccess.fieldSelection.field = IR_FieldCollection.getByIdentifier(newName, transformedFieldAccess.fieldSelection.field.level).get
      }
      case _ =>
    }

    transformedFieldAccess
  }
}

object IR_CommTransformationCollection{
  var trafos : ListBuffer[IR_CommTransformation] = ListBuffer[IR_CommTransformation]()

  def setup() = {
    trafos.clear()

    if(Knowledge.dimensionality == 2){
      // setup all trafos for 2D
      for((_, i) <- IR_CommTrafoCollection.trafoArray.zipWithIndex)
        trafos = trafos :+ IR_CommTransformation(Knowledge.dimensionality, i)
    }
    else{
      Logger.error("IR_CommTransformationCollection cannot deal with dimensionality " + Knowledge.dimensionality)
    }
  }

}