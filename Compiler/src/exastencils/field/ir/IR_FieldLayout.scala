package exastencils.field.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_Linearization
import exastencils.config._
import exastencils.grid.ir.IR_Localization
import exastencils.knowledge.ir._
import exastencils.logger.Logger

/// IR_FieldLayout

case class IR_FieldLayout(
    var name : String, // will be used to find the field
    var level : Int, // the (geometric) level the layout is associated with
    var datatype : IR_Datatype, // represents the (original) data type; may be multidimensional, i.e. vectors, matrices, etc.
    var localization : IR_Localization, // specifies where data is located
    var layoutsPerDim : Array[IR_FieldLayoutPerDim], // represents the number of data points and their distribution in each dimension
    var numDimsGrid : Int, // dimensionality of the associated grid; usually lesser than or equal to 3
    var referenceOffset : IR_ExpressionIndex, // specifies the (index) offset from the lower corner of the field to the first reference point; in case of node-centered data points the reference point is the first vertex point
    var communicatesDuplicated : Boolean, // specifies if duplicated values need to be exchanged between processes
    var communicatesGhosts : Boolean // specifies if ghost layer values need to be exchanged between processes
) extends IR_LeveledKnowledgeObject {

  // dimensionality of the stored data; numDimsGrid for scalar fields, numDimsGrid + 1 for vector fields, numDimsGrid + 2 for matrix fields, etc.
  def numDimsData : Int = layoutsPerDim.length

  def apply(dim : Int) = layoutsPerDim(dim)

  def defIdxPadLeftBegin(dim : Int) = { 0 }
  def defIdxPadLeftEnd(dim : Int) = { defIdxPadLeftBegin(dim) + layoutsPerDim(dim).numPadLayersLeft }

  def defIdxGhostLeftBegin(dim : Int) = { defIdxPadLeftBegin(dim) + layoutsPerDim(dim).numPadLayersLeft }
  def defIdxGhostLeftEnd(dim : Int) = { defIdxGhostLeftBegin(dim) + layoutsPerDim(dim).numGhostLayersLeft }

  def defIdxDupLeftBegin(dim : Int) = { defIdxGhostLeftBegin(dim) + layoutsPerDim(dim).numGhostLayersLeft }
  def defIdxDupLeftEnd(dim : Int) = { defIdxDupLeftBegin(dim) + layoutsPerDim(dim).numDupLayersLeft }

  def defIdxInnerBegin(dim : Int) = { defIdxDupLeftBegin(dim) + layoutsPerDim(dim).numDupLayersLeft }
  def defIdxInnerEnd(dim : Int) = { defIdxInnerBegin(dim) + layoutsPerDim(dim).numInnerLayers }

  def defIdxDupRightBegin(dim : Int) = { defIdxInnerBegin(dim) + layoutsPerDim(dim).numInnerLayers }
  def defIdxDupRightEnd(dim : Int) = { defIdxDupRightBegin(dim) + layoutsPerDim(dim).numDupLayersRight }

  def defIdxGhostRightBegin(dim : Int) = { defIdxDupRightBegin(dim) + layoutsPerDim(dim).numDupLayersRight }
  def defIdxGhostRightEnd(dim : Int) = { defIdxGhostRightBegin(dim) + layoutsPerDim(dim).numGhostLayersRight }

  def defIdxPadRightBegin(dim : Int) = { defIdxGhostRightBegin(dim) + layoutsPerDim(dim).numGhostLayersRight }
  def defIdxPadRightEnd(dim : Int) = { defIdxPadRightBegin(dim) + layoutsPerDim(dim).numPadLayersRight }

  def defTotal(dim : Int) = { defIdxPadRightEnd(dim) }

  def idxById(id : String, dim : Int) : IR_Expression = {
    if (Knowledge.data_genVariableFieldSizes && dim < Knowledge.dimensionality)
    // TODO : total
      IR_IV_IndexFromField(name, level, id, dim)
    else
      defIdxById(id, dim)
  }

  def defIdxById(id : String, dim : Int) : Int = {
    id match {
      case "PLB" => defIdxPadLeftBegin(dim)
      case "PLE" => defIdxPadLeftEnd(dim)
      case "GLB" => defIdxGhostLeftBegin(dim)
      case "GLE" => defIdxGhostLeftEnd(dim)
      case "DLB" => defIdxDupLeftBegin(dim)
      case "DLE" => defIdxDupLeftEnd(dim)
      case "IB"  => defIdxInnerBegin(dim)
      case "IE"  => defIdxInnerEnd(dim)
      case "DRB" => defIdxDupRightBegin(dim)
      case "DRE" => defIdxDupRightEnd(dim)
      case "GRB" => defIdxGhostRightBegin(dim)
      case "GRE" => defIdxGhostRightEnd(dim)
      case "PRB" => defIdxPadRightBegin(dim)
      case "PRE" => defIdxPadRightEnd(dim)
      case "TOT" => defTotal(dim)
    }
  }

  def updateDefReferenceOffset() = {
    // TODO: this should work for now but may be adapted in the future
    referenceOffset = IR_ExpressionIndex(Array.fill(layoutsPerDim.length)(0))
    for (dim <- 0 until layoutsPerDim.length)
      referenceOffset(dim) = IR_IntegerConstant(layoutsPerDim(dim).numPadLayersLeft + layoutsPerDim(dim).numGhostLayersLeft)
  }

  def linearizeIndex(index : IR_Index) : IR_Expression = {
    if (numDimsData != index.length())
      Logger.warn(s"Index length mismatch for $name@$level: ${ index.length() }, should be $numDimsData")
    IR_Linearization.linearizeIndex(index, IR_ExpressionIndex((0 until math.min(numDimsData, index.length())).map(idxById("TOT", _)).toArray))
  }
}

case class IR_FieldLayoutPerDim(
    var numPadLayersLeft : Int, // number of padding data points added to the left (/ lower / front) side of the field
    var numGhostLayersLeft : Int, // number of ghost data points added to the left (/ lower / front) side of the field used for communication
    var numDupLayersLeft : Int, // number of duplicated data points added to the left (/ lower / front) side of the field; will usually be treated as inner points; can be directly communicated to/ from the opposite dup layers
    var numInnerLayers : Int, // number of inner data points (per dimension); don't include duplicated points (conceptually)
    var numDupLayersRight : Int, // number of duplicated data points added to the right (/ upper / back) side of the field; will usually be treated as inner points; can be directly communicated to/ from the opposite dup layers
    var numGhostLayersRight : Int, // number of ghost data points added to the right (/ upper / back) side of the field used for communication
    var numPadLayersRight : Int // number of padding data points added to the right (/ upper / back) side of the field
) {
  var total : IR_Expression = "NOT SET"

  // update total at initialization
  updateTotal()

  def updateTotal() = {
    total = numPadLayersLeft + numGhostLayersLeft + numDupLayersLeft + numInnerLayers + numDupLayersRight + numGhostLayersRight + numPadLayersRight
  }

  def isSingleValued() : Boolean = {
    numPadLayersLeft == 0 &&
      numGhostLayersLeft == 0 &&
      numDupLayersLeft == 0 &&
      numInnerLayers == 1 &&
      numDupLayersRight == 0 &&
      numGhostLayersRight == 0 &&
      numPadLayersRight == 0
  }
}
