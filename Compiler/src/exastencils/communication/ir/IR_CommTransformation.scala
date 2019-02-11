package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression

case class IR_CommTransformation(var dim : Int, var trafoId : Int) {
  // 0 --> W
  // 1 --> E
  // 2 --> S
  // 3 --> N

  def transformIndex(index : Array[IR_Expression], indexSize : Array[Long], indexBegin : Array[Long]) : Array[IR_Expression] = {

    def mirrorIndex(dim : Int) : IR_Expression = {
      indexSize(dim) - index(dim) + 2 * indexBegin(dim)
    }

    var newIndex = new Array[IR_Expression](2)

    newIndex(0) = trafoId match {
      case 0 | 1 => index(0)
      case 2 | 3 => mirrorIndex(0)
    }
    newIndex(1) = trafoId match {
      case 0 | 3 => index(1)
      case 1 | 2 => mirrorIndex(1)
    }

    newIndex
  }

  def switchUL(fieldName : String) = {
    fieldName.dropRight(1) + (fieldName.last match {
      case 'U' => 'L'
      case 'L' => 'U'
      case o   => o
    })
  }

  def applyTrafo(fieldAccess : IR_DirectFieldAccess, thisIndexRange : IR_ExpressionIndexRange, neigh : NeighborInfo) = {
    var indexSize = (thisIndexRange.end - thisIndexRange.begin).indices.map(IR_SimplifyExpression.evalIntegral).map(_ - 1)
    var indexBegin = thisIndexRange.begin.indices.map(IR_SimplifyExpression.evalIntegral)

    val index = fieldAccess.index

    val trafoIndex = IR_ExpressionIndex(transformIndex(index.indices, indexSize, indexBegin)
      ++ index.drop(fieldAccess.fieldSelection.field.numDimsGrid)
    )

    val transformedFieldAccess = IR_DirectFieldAccess(fieldAccess.fieldSelection, trafoIndex)

    def origField = fieldAccess.fieldSelection.field
    if (IR_FieldCombinationCollection.existsInCombination(origField, "Triangles")) {
      trafoId match {
        // TODO check that U/L-switch must be done in these cases
        case 2 | 3 =>

          val combinations = IR_FieldCombinationCollection.getByFieldInCombination(origField, "Triangles")
          if (combinations.length > 1)
            Logger.error(s"Found triangle field ${ origField.name } in more than one combination; unsupported")
          if (combinations.head.fields.length != 2)
            Logger.error(s"Found triangle combination with more than one field; unsupported")

          val newField = combinations.head.fields.filterNot(_ == origField).head
          transformedFieldAccess.fieldSelection.field = newField

        case _ =>
      }
    }

    transformedFieldAccess
  }
}

object IR_CommTransformationCollection {
  var trafos : ListBuffer[IR_CommTransformation] = ListBuffer[IR_CommTransformation]()

  def setup() = {
    trafos.clear()

    if (Knowledge.dimensionality == 2) {
      // setup all trafos for 2D
      for ((_, i) <- IR_CommTrafoCollection.trafoArray.zipWithIndex)
        trafos = trafos :+ IR_CommTransformation(Knowledge.dimensionality, i)
    }
    else {
      Logger.error("IR_CommTransformationCollection cannot deal with dimensionality " + Knowledge.dimensionality)
    }
  }

}