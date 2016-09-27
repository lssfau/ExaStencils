package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.boundary.ir.IR_BoundaryCondition
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.logger._

case class FieldLayout(
    var identifier : String, // will be used to find the field
    var level : Int, // the (geometric) level the layout is associated with
    var datatype : IR_Datatype, // represents the (original) data type; may be multidimensional, i.e. vectors, matrices, etc.
    var discretization : String, // specifies where data is located; currently allowed values are "node", "cell" and "face_{x,y,z}"
    var layoutsPerDim : Array[FieldLayoutPerDim], // represents the number of data points and their distribution in each dimension
    var numDimsGrid : Int, // dimensionality of the associated grid; usually lesser than or equal to 3
    var numDimsData : Int, // dimensionality of the stored data; numDimsGrid for scalar fields, numDimsGrid + 1 for vector fields, numDimsGrid + 2 for matrix fields, etc.
    var referenceOffset : IR_ExpressionIndex, // specifies the (index) offset from the lower corner of the field to the first reference point; in case of node-centered data points the reference point is the first vertex point
    var communicatesDuplicated : Boolean, // specifies if duplicated values need to be exchanged between processes
    var communicatesGhosts : Boolean // specifies if ghost layer values need to be exchanged between processes
) extends IR_KnowledgeObject {
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
      iv.IndexFromField(identifier, level, id, dim)
    // TODO : total
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
  var total : IR_Expression = "NOT SET"

  // update total at initialization
  updateTotal()

  def updateTotal() = {
    total = numPadLayersLeft + numGhostLayersLeft + numDupLayersLeft + numInnerLayers + numDupLayersRight + numGhostLayersRight + numPadLayersRight
  }
}

object FieldLayoutCollection {
  exastencils.core.Duplicate.registerConstant(this)
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[FieldLayout])
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[FieldLayoutPerDim])
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
    var boundary : IR_BoundaryCondition // the boundary condition to be enforced when calling apply bc
) extends IR_KnowledgeObject {
  // shortcuts to layout options
  def gridDatatype = fieldLayout.datatype
  def resolveBaseDatatype = fieldLayout.datatype.resolveBaseDatatype
  def resolveDeclType = fieldLayout.datatype.resolveDeclType
  def discretization = fieldLayout.discretization
  def referenceOffset = fieldLayout.referenceOffset
  def communicatesDuplicated = fieldLayout.communicatesDuplicated
  def communicatesGhosts = fieldLayout.communicatesGhosts
}

case class FieldSelection(
    var field : Field,
    var level : IR_Expression,
    var slot : IR_Expression,
    var arrayIndex : Option[Int] = None, // TODO: delete
    var fragIdx : IR_Expression = IR_LoopOverFragments.defIt) extends Node {

  // shortcuts to Field members
  def codeName = field.codeName
  def fieldLayout = field.fieldLayout
  def referenceOffset = field.referenceOffset

  // other shortcuts
  def domainIndex = field.domain.index
}

object FieldCollection {
  exastencils.core.Duplicate.registerConstant(this)
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[Field])
  var fields : ListBuffer[Field] = ListBuffer()

  def getSortedFields : ListBuffer[Field] = {
    fields.sortBy(f => (f.identifier, f.level))
  }

  def getFieldByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[Field] = {
    val ret = fields.find(f => f.identifier == identifier && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"Field $identifier on level $level was not found")
    ret
  }

  def getFieldByIdentifierLevExp(identifier : String, level : IR_Expression, suppressError : Boolean = false) : Option[Field] = {
    level match {
      case IR_IntegerConstant(constLevel) => getFieldByIdentifier(identifier, constLevel.toInt, suppressError)
      case _                              => {
        if (!suppressError) Logger.warn(s"Trying to find field $identifier on level ${ level.prettyprint } - non-constant levels are not supported")
        None
      }
    }
  }

  def getFieldByLayoutIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[Field] = {
    val ret = fields.find(f => f.fieldLayout.identifier == identifier && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"Field with layout $identifier on level $level was not found")
    ret
  }
}

case class ExternalField(
    var identifier : String, // will be used to find the field
    var targetField : Field, // the (internal) field to be copied to/ from
    var fieldLayout : FieldLayout, // represents the number of data points and their distribution in each dimension
    var level : Int // the (geometric) level the field lives on
) extends IR_KnowledgeObject {
  // shortcuts to layout options
  def gridDatatype = fieldLayout.datatype
  def resolveBaseDatatype = fieldLayout.datatype.resolveBaseDatatype
  def resolveDeclType = fieldLayout.datatype.resolveDeclType
  def referenceOffset = fieldLayout.referenceOffset
  def communicatesDuplicated = fieldLayout.communicatesDuplicated
  def communicatesGhosts = fieldLayout.communicatesGhosts
}

object ExternalFieldCollection {
  exastencils.core.Duplicate.registerConstant(this)
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[ExternalField])
  var fields : ListBuffer[ExternalField] = ListBuffer()

  def getSortedFields : ListBuffer[ExternalField] = {
    fields.sortBy(f => (f.identifier, f.level))
  }

  def getFieldByIdentifier(identifier : String, level : Int) : Option[ExternalField] = {
    val ret = fields.find(f => f.identifier == identifier && f.level == level)
    if (ret.isEmpty) Logger.warn(s"External field $identifier on level $level was not found")
    ret
  }

  def getFieldByLayoutIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[ExternalField] = {
    val ret = fields.find(f => f.fieldLayout.identifier == identifier && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"External field with layout $identifier on level $level was not found")
    ret
  }
}
