package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.core.Logger._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

class FieldLayoutPerDim(
    var numPadLayersLeft : Int, // number of padding data points added to the left (/ lower / front) side of the field
    var numGhostLayersLeft : Int, // number of ghost data points added to the left (/ lower / front) side of the field used for communication
    var numDupLayersLeft : Int, // number of duplicated data points added to the left (/ lower / front) side of the field; will usually be treated as inner points; can be directly communicated to/ from the opposite dup layers
    var numInnerLayers : Int, // number of inner data points (per dimension); don't include duplicated points (conceptionally)
    var numDupLayersRight : Int, // number of duplicated data points added to the right (/ upper / back) side of the field; will usually be treated as inner points; can be directly communicated to/ from the opposite dup layers
    var numGhostLayersRight : Int, // number of ghost data points added to the right (/ upper / back) side of the field used for communication
    var numPadLayersRight : Int // number of padding data points added to the right (/ upper / back) side of the field
    ) {

  def idxPadLeftBegin = { 0 }
  def idxPadLeftEnd = { idxPadLeftBegin + numPadLayersLeft }

  def idxGhostLeftBegin = { idxPadLeftBegin + numPadLayersLeft }
  def idxGhostLeftEnd = { idxGhostLeftBegin + numGhostLayersLeft }

  def idxDupLeftBegin = { idxGhostLeftBegin + numGhostLayersLeft }
  def idxDupLeftEnd = { idxDupLeftBegin + numDupLayersLeft }

  def idxInnerBegin = { idxDupLeftBegin + numDupLayersLeft }
  def idxInnerEnd = { idxInnerBegin + numInnerLayers }

  def idxDupRightBegin = { idxInnerBegin + numInnerLayers }
  def idxDupRightEnd = { idxDupRightBegin + numDupLayersRight }

  def idxGhostRightBegin = { idxDupRightBegin + numDupLayersRight }
  def idxGhostRightEnd = { idxGhostRightBegin + numGhostLayersRight }

  def idxPadRightBegin = { idxGhostRightBegin + numGhostLayersRight }
  def idxPadRightEnd = { idxPadRightBegin + numPadLayersRight }

  def total = { idxPadRightBegin + numPadLayersRight }
}

case class Field(
    var identifier : String, // will be used to find the field
    var index : Int, // (consecutive) index of the field, can be used as array subscript
    var domain : Domain, // the (sub)domain the field lives on
    var codeName : String, // will be used in the generated source code
    var dataType : Datatype, // represents the data type; thus it can also encode the dimensionality when using e.g. vector fields
    var layout : Array[FieldLayoutPerDim], // represents the number of data points and their distribution in each dimension
    var communicatesDuplicated : Boolean, // specifies if duplicated values need to be exchanged between processes
    var communicatesGhosts : Boolean, // specifies if ghost layer values need to be exchanged between processes
    var level : Int, // the (geometric) level the field lives on 
    var numSlots : Int, // the number of copies of the field to be available; can be used to represent different vector components or different versions of the same field (e.g. Jacobi smoothers, time-stepping)
    var referenceOffset : MultiIndex, // specifies the (index) offset from the lower corner of the field to the first reference point; in case of node-centered data points the reference point is the first vertex point
    var dirichletBC : Option[Expression], // None in case of no dirichlet BC, otherwise specifies the expression to be used for the dirichlet boundary
    var alignmentPadding : Expression // specifies an additional padding at the beginning of the field in order to ensure correct alignment for SIMD accesses
    ) {
  def vectorSize = dataType.resolveFlattendSize
}

case class FieldSelection(
    var field : Field,
    var slot : Expression,
    var arrayIndex : Int,
    var fragIdx : Expression = LoopOverFragments.defIt) extends Node {

  // shortcuts to Field members
  def codeName = field.codeName
  def dataType = field.dataType
  def layout = field.layout
  def level = field.level
  def referenceOffset = field.referenceOffset

  // other shortcuts
  def domainIndex = field.domain.index
}

object FieldCollection {
  var fields : ListBuffer[Field] = ListBuffer()

  def getFieldByIdentifier(identifier : String, level : Int) : Option[Field] = {
    val ret = fields.find(f => f.identifier == identifier && f.level == level)
    if (ret.isEmpty) warn(s"Field $identifier on level $level was not found")
    ret
  }
}

case class ExternalField(
  var identifier : String, // will be used to find the field
  var targetField : Field, // the (internal) field to be copied to/ from
  var layout : Array[FieldLayoutPerDim], // represents the number of data points and their distribution in each dimension
  var level : Int, // the (geometric) level the field lives on 
  var referenceOffset : MultiIndex // specifies the (index) offset from the lower corner of the field to the first reference point; in case of node-centered data points the reference point is the first vertex point
  ) {}

object ExternalFieldCollection {
  var fields : ListBuffer[ExternalField] = ListBuffer()

  def getFieldByIdentifier(identifier : String, level : Int) : Option[ExternalField] = {
    val ret = fields.find(f => f.identifier == identifier && f.level == level)
    if (ret.isEmpty) warn(s"External field $identifier on level $level was not found")
    ret
  }
}
