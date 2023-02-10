//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.ir._
import exastencils.fieldlike.ir.IR_DirectFieldLikeAccess
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression

case class IR_CommTransformation(var dim : Int, var trafoId : Int) {
  // 0 --> W
  // 1 --> E
  // 2 --> S
  // 3 --> N

  def switchUL(fieldAccess : IR_DirectFieldLikeAccess, neigh : NeighborInfo) = {
    def origField = fieldAccess.field

    var replacementField = origField
    if (IR_FieldCombinationCollection.existsInCombination(origField, "Triangles")) {
      (trafoId, neigh.index) match {
        case (1, 2) | (1, 3) | (2, _) | (3, 0) | (3, 1) =>

          val combinations = IR_FieldCombinationCollection.getByFieldInCombination(origField, "Triangles")
          if (combinations.length > 1)
            Logger.error(s"Found triangle field ${ origField.name } in more than one combination; unsupported")
          if (combinations.head.fields.length != 2)
            Logger.error(s"Found triangle combination with more than one field; unsupported")

          val newField = combinations.head.fields.filterNot(_ == origField).head
          replacementField = newField

        case _ =>
      }
    }

    IR_DirectFieldLikeAccess(replacementField, fieldAccess.slot, fieldAccess.fragIdx, fieldAccess.index)
  }

  def applyRemoteTrafo(fieldAccess : IR_DirectFieldLikeAccess, indexRange : IR_ExpressionIndexRange, neigh : NeighborInfo) = {

    def transformIndex(index : Array[IR_Expression], indexSize : Array[Long], indexBegin : Array[Long]) : Array[IR_Expression] = {

      def mirrorIndex(dim : Int) : IR_Expression = indexSize(dim) - index(dim) + 2 * indexBegin(dim)

      val newIndex = new Array[IR_Expression](2)

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

    val indexSize = (indexRange.end - indexRange.begin).indices.map(IR_SimplifyExpression.evalIntegral).map(_ - 1)
    val indexBegin = indexRange.begin.indices.map(IR_SimplifyExpression.evalIntegral)

    val index = fieldAccess.index

    val trafoIndex = IR_ExpressionIndex(transformIndex(index.indices, indexSize, indexBegin)
      ++ index.drop(fieldAccess.field.numDimsGrid)
    )

    val transformedFieldAccess = IR_DirectFieldLikeAccess(fieldAccess.field, Duplicate(fieldAccess.slot), Duplicate(fieldAccess.fragIdx), trafoIndex)

    switchUL(transformedFieldAccess, neigh)
  }

  def applyBufferTrafo(bufferAccess : IR_TempBufferAccess) = {
    trafoId match {
      case 1 | 3 =>
        val strides = bufferAccess.strides
        val trafoStrides = IR_ExpressionIndex(Array[IR_Expression](
          strides(1),
          strides(0)
        ) ++ strides.drop(2))
        val index = bufferAccess.index
        val trafoIndex = IR_ExpressionIndex(Array[IR_Expression](
          index(1),
          index(0)
        ) ++ index.drop(2))
        IR_TempBufferAccess(bufferAccess.buffer, trafoIndex, trafoStrides)

      case _ => bufferAccess
    }
  }

  def applyLocalTrafo(fieldAccess : IR_DirectFieldLikeAccess, neigh : NeighborInfo) = {
    def fieldSize(i : Int) = fieldAccess.field.layout.defTotal(i) - 1

    val rot90mat = Array(Array(0, -1), Array(1, 0))

    def mult(a : Array[Array[Int]], b : Array[Array[Int]]) = {
      for (row <- a)
        yield for (col <- b.transpose)
          yield row zip col map Function.tupled(_ * _) reduceLeft (_ + _)
    }

    var rotMat = Array(Array(1, 0), Array(0, 1))
    for (_ <- 1 to trafoId)
      rotMat = mult(rotMat, rot90mat)

    // Compute translation of origin
    val t : Array[IR_Expression] = Array(0, 0)
    trafoId match {
      case 0 =>
      case 1 =>
        t(0) = fieldSize(0)
      case 2 =>
        t(0) = fieldSize(0)
        t(1) = fieldSize(1)
      case 3 =>
        t(1) = fieldSize(1)
    }

    // Compute new index: R * i + t
    val trafoIndices = Array(
      rotMat(0)(0) * fieldAccess.index(0) + rotMat(0)(1) * fieldAccess.index(1) + t(0),
      rotMat(1)(0) * fieldAccess.index(0) + rotMat(1)(1) * fieldAccess.index(1) + t(1)
    ) ++ fieldAccess.index.drop(2)

    val transformedFieldAccess = IR_DirectFieldLikeAccess(fieldAccess.field, Duplicate(fieldAccess.slot), Duplicate(fieldAccess.fragIdx), IR_ExpressionIndex(trafoIndices))

    switchUL(transformedFieldAccess, neigh)
  }
}

object IR_CommTransformationCollection {
  var trafos : ListBuffer[IR_CommTransformation] = ListBuffer[IR_CommTransformation]()

  def setup() = {
    trafos.clear()

    if (Knowledge.dimensionality == 2) {
      // setup all trafos for 2D
      for ((_, i) <- IR_CommTrafoIdCollection.trafoArray.zipWithIndex)
        trafos = trafos :+ IR_CommTransformation(Knowledge.dimensionality, i)
    }
    else {
      Logger.error("IR_CommTransformationCollection cannot deal with dimensionality " + Knowledge.dimensionality)
    }
  }

}

object IR_CommTrafoIdCollection {
  // stores relation between own neighborIdx and neighborIdx of the neighbor
  val trafoArray : Array[List[(Int, Int)]] = Array(
    List((0, 1), (2, 3), (1, 0), (3, 2)), // 0
    List((0, 3), (2, 0), (1, 2), (3, 1)), // 1
    List((0, 0), (2, 2), (1, 1), (3, 3)), // 2
    List((0, 2), (2, 1), (1, 3), (3, 0)) // 3
  )
}
