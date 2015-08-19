package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.logger._

case class FieldLayout(
    var identifier : String, // will be used to find the field
    var level : Int, // the (geometric) level the layout is associated with 
    var dataType : Datatype, // represents the data type; thus it can also encode the dimensionality when using e.g. vector fields
    var discretization : String, // specifies where data is located; currently allowed values are "node", "cell" and "face_{x,y,z}" 
    var layoutsPerDim : Array[FieldLayoutPerDim], // represents the number of data points and their distribution in each dimension
    var referenceOffset : MultiIndex, // specifies the (index) offset from the lower corner of the field to the first reference point; in case of node-centered data points the reference point is the first vertex point
    var communicatesDuplicated : Boolean, // specifies if duplicated values need to be exchanged between processes
    var communicatesGhosts : Boolean // specifies if ghost layer values need to be exchanged between processes
    ) {
  def apply(dim : Int) = layoutsPerDim(dim)
}

case class FieldLayoutPerDim(
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

  def evalTotal = { idxPadRightEnd }
  var total : Expression = evalTotal

  def idxById(id : String) : Expression = {
    id match {
      case "PLB" => idxPadLeftBegin
      case "PLE" => idxPadLeftEnd
      case "GLB" => idxGhostLeftBegin
      case "GLE" => idxGhostLeftEnd
      case "DLB" => idxDupLeftBegin
      case "DLE" => idxDupLeftEnd
      case "IB"  => idxInnerBegin
      case "IE"  => idxInnerEnd
      case "DRB" => idxDupRightBegin
      case "DRE" => idxDupRightEnd
      case "GRB" => idxGhostRightBegin
      case "GRE" => idxGhostRightEnd
      case "PRB" => idxPadRightBegin
      case "PRE" => idxPadRightEnd
      case "TOT" => total
    }
  }
}

object FieldLayoutCollection {
  var fieldLayouts : ListBuffer[FieldLayout] = ListBuffer()

  def getFieldLayoutByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[FieldLayout] = {
    val ret = fieldLayouts.find(f => f.identifier == identifier && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"FieldLayout $identifier on level $level was not found")
    ret
  }
}

case class Field(
    var identifier : String, // will be used to find the field
    var index : Int, // (consecutive) index of the field, can be used as array subscript
    var domain : Domain, // the (sub)domain the field lives on
    var codeName : String, // will be used in the generated source code
    var fieldLayout : FieldLayout, // represents the number of data points and their distribution in each dimension
    var level : Int, // the (geometric) level the field lives on 
    var numSlots : Int, // the number of copies of the field to be available; can be used to represent different vector components or different versions of the same field (e.g. Jacobi smoothers, time-stepping)
    var boundaryConditions : Option[Expression] // None if no explicit boundary handling is given, otherwise specifies the expression to be used for the dirichlet boundary or Neumann as magic identifier
    ) {
  // shortcuts to layout options  
  def dataType = fieldLayout.dataType
  def discretization = fieldLayout.discretization
  def vectorSize = fieldLayout.dataType.resolveFlattendSize
  def referenceOffset = fieldLayout.referenceOffset
  def communicatesDuplicated = fieldLayout.communicatesDuplicated
  def communicatesGhosts = fieldLayout.communicatesGhosts
}

case class FieldSelection(
    var field : Field,
    var level : Expression,
    var slot : Expression,
    var arrayIndex : Option[Int] = None,
    var fragIdx : Expression = LoopOverFragments.defIt) extends Node {

  // shortcuts to Field members
  def codeName = field.codeName
  def dataType = field.dataType
  def fieldLayout = field.fieldLayout
  def referenceOffset = field.referenceOffset

  // other shortcuts
  def domainIndex = field.domain.index
}

object FieldCollection {
  var fields : ListBuffer[Field] = ListBuffer()

  def getSortedFields : ListBuffer[Field] = {
    fields.sortBy(_.level).sortBy(_.identifier)
  }

  def getFieldByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[Field] = {
    val ret = fields.find(f => f.identifier == identifier && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"Field $identifier on level $level was not found")
    ret
  }

  def getFieldByIdentifierLevExp(identifier : String, level : Expression, suppressError : Boolean = false) : Option[Field] = {
    level match {
      case IntegerConstant(constLevel) => getFieldByIdentifier(identifier, constLevel.toInt, suppressError)
      case _ => {
        if (suppressError) Logger.warn(s"Trying to find field $identifier on level ${level.prettyprint} - non-constant levels are not supported")
        None
      }
    }
  }
}

case class ExternalField(
    var identifier : String, // will be used to find the field
    var targetField : Field, // the (internal) field to be copied to/ from
    var fieldLayout : FieldLayout, // represents the number of data points and their distribution in each dimension
    var level : Int // the (geometric) level the field lives on 
    ) {
  // shortcuts to layout options  
  def dataType = fieldLayout.dataType
  def vectorSize = fieldLayout.dataType.resolveFlattendSize
  def referenceOffset = fieldLayout.referenceOffset
  def communicatesDuplicated = fieldLayout.communicatesDuplicated
  def communicatesGhosts = fieldLayout.communicatesGhosts
}

object ExternalFieldCollection {
  var fields : ListBuffer[ExternalField] = ListBuffer()

  def getSortedFields : ListBuffer[ExternalField] = {
    fields.sortBy(_.level).sortBy(_.identifier)
  }

  def getFieldByIdentifier(identifier : String, level : Int) : Option[ExternalField] = {
    val ret = fields.find(f => f.identifier == identifier && f.level == level)
    if (ret.isEmpty) Logger.warn(s"External field $identifier on level $level was not found")
    ret
  }
}
