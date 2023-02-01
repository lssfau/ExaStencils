package exastencils.fieldlike.ir

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_Linearization
import exastencils.field.ir.IR_FieldLayoutPerDim
import exastencils.grid.ir.IR_Localization
import exastencils.knowledge.ir.IR_LeveledKnowledgeObject
import exastencils.logger.Logger

/// IR_FieldLayoutLike

trait IR_FieldLikeLayout extends IR_LeveledKnowledgeObject {
  def name : String // will be used to find the field
  def level : Int // the (geometric) level the layout is associated with
  def datatype : IR_Datatype // represents the (original) data type; may be multidimensional, i.e. vectors, matrices, etc.
  def localization : IR_Localization // specifies where data is located
  def layoutsPerDim : Array[IR_FieldLayoutPerDim] // represents the number of data points and their distribution in each dimension
  def numDimsGrid : Int // dimensionality of the associated grid; usually lesser than or equal to 3
  def referenceOffset : IR_ExpressionIndex // specifies the (index) offset from the lower corner of the field to the first reference point; in case of node-centered data points the reference point is the first vertex point
  def communicatesDuplicated : Boolean // specifies if duplicated values need to be exchanged between processes
  def communicatesGhosts : Boolean // specifies if ghost layer values need to be exchanged between processes

  def defIdxById(id : String, dim : Int) : IR_Expression
  def idxById(id : String, dim : Int) : IR_Expression

  def updateDefReferenceOffset() : Unit

  // dimensionality of the stored data; numDimsGrid for scalar fields, numDimsGrid + 1 for vector fields, numDimsGrid + 2 for matrix fields, etc.
  def numDimsData : Int

  def apply(dim : Int) = layoutsPerDim(dim)

  def defTotal(dim : Int) : IR_Expression

  def linearizeIndex(index : IR_Index) : IR_Expression = {
    if (numDimsData != index.length())
      Logger.warn(s"Index length mismatch for $name@$level: ${ index.length() }, should be $numDimsData")
    IR_Linearization.linearizeIndex(index, IR_ExpressionIndex((0 until math.min(numDimsData, index.length())).map(idxById("TOT", _)).toArray))
  }
}
