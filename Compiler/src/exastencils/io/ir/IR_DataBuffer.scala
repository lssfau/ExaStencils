package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_AddressOf
import exastencils.base.ir.IR_BooleanConstant
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Index
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_Negative
import exastencils.base.ir.IR_NullExpression
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_DomainCollection
import exastencils.domain.ir.IR_IV_FragmentIndex
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.grid.ir.IR_Localization
import exastencils.logger.Logger

object IR_DataBuffer {

  // accepts a field as input and retrieves dims, localization, etc. from it
  def apply(field : IR_Field, slot : IR_Expression, includeGhosts : Boolean, pattern : Option[IR_AccessPattern], dataset : Option[IR_Expression]) = new IR_DataBuffer(
    slot = slot,
    datatype = field.gridDatatype,
    localization = field.localization,
    referenceOffset = field.referenceOffset,
    beginIndices = (0 until field.layout.numDimsData).map(d => field.layout.defIdxById(if(includeGhosts) "GLB" else "DLB", d) : IR_Expression).to[ListBuffer],
    endIndices = (0 until field.layout.numDimsData).map(d => field.layout.defIdxById(if(includeGhosts) "GRE" else "DRE", d) : IR_Expression).to[ListBuffer],
    totalDimsLocal = (0 until field.layout.numDimsData).map(d => field.layout.defTotal(d) : IR_Expression).to[ListBuffer],
    numDimsGrid = field.layout.numDimsGrid,
    numDimsData = field.layout.numDimsData,
    domainIdx = field.domain.index,
    name = field.name,
    accessPattern = pattern getOrElse IR_AccessPattern((idx : IR_Index) => IR_FieldAccess(field, Duplicate(slot), idx.toExpressionIndex)),
    datasetName = dataset getOrElse IR_NullExpression,
    accessBlockwise = false,
    isDiscField = false
  )

  // assumes the temporary buffer only contains the values of interest (e.g. no padding/ghost layers to exclude)
  def apply(tmpBuf : IR_IV_TemporaryBuffer, slot : IR_Expression, localization: IR_Localization, pattern : Option[IR_AccessPattern], dataset : Option[IR_Expression]) : IR_DataBuffer = new IR_DataBuffer(
    slot = slot,
    datatype = tmpBuf.resolveDatatype(),
    localization = localization,
    referenceOffset = IR_ExpressionIndex(Array.fill(tmpBuf.numDims)(0)),
    beginIndices = ListBuffer.fill(tmpBuf.numDims)(0),
    endIndices = tmpBuf.dims,
    totalDimsLocal = tmpBuf.dims,
    numDimsGrid = tmpBuf.dims.length,
    numDimsData = tmpBuf.dims.length,
    domainIdx = IR_DomainCollection.getByIdentifier("global").get.index,
    name = tmpBuf.name,
    accessPattern = pattern.getOrElse(IR_AccessPattern((idx : IR_Index) => tmpBuf.resolveAccess(idx))),
    datasetName = dataset getOrElse IR_NullExpression,
    accessBlockwise = true,
    isDiscField = false
  )

  // special case for SWE (e.g. etaDiscLower0, ...)
  def apply(discField : ListBuffer[IR_Field], slot : IR_Expression, pattern : Option[IR_AccessPattern], dataset : Option[IR_Expression]) : IR_DataBuffer = {
    val field = discField.head
    val idxRange = 0 until field.layout.numDimsData
    new IR_DataBuffer(
      slot = slot,
      datatype = field.gridDatatype,
      localization = field.layout.localization,
      referenceOffset = field.referenceOffset,
      beginIndices = idxRange.map(d => field.layout.defIdxById("IB", d) : IR_Expression).to[ListBuffer],
      endIndices = idxRange.map(d => field.layout.defIdxById("IE", d) : IR_Expression).to[ListBuffer],
      totalDimsLocal = idxRange.map(d => field.layout.defTotal(d) : IR_Expression).to[ListBuffer],
      numDimsGrid = field.layout.numDimsGrid,
      numDimsData = field.layout.numDimsData,
      domainIdx = field.domain.index,
      accessPattern = pattern getOrElse IR_AccessPattern((idx : IR_Index) => IR_FieldAccess(field, Duplicate(slot), idx.toExpressionIndex)),
      datasetName = dataset getOrElse IR_NullExpression,
      name = field.name,
      accessBlockwise = false,
      isDiscField = true
    )
  }
}

case class IR_DataBuffer(
    var slot : IR_Expression,
    var datatype : IR_Datatype,
    var localization : IR_Localization,
    var accessPattern : IR_AccessPattern,
    var referenceOffset : IR_ExpressionIndex,
    var beginIndices : ListBuffer[IR_Expression],
    var endIndices : ListBuffer[IR_Expression],
    var totalDimsLocal : ListBuffer[IR_Expression],
    var numDimsGrid : Int,
    var numDimsData : Int,
    var domainIdx : Int,
    var name : String,
    var datasetName : IR_Expression,
    var accessBlockwise : Boolean, // TODO do we actually need to specify this explicitly?
    var isDiscField : Boolean // TODO
) {

  def stride : ListBuffer[IR_Expression] = accessPattern.stridePerDimension.getOrElse(numDimsDataRange.map(_ => 1 : IR_Expression).to[ListBuffer])

  def numDimsGridRange : Range = 0 until numDimsGrid
  def numDimsDataRange : Range = 0 until numDimsData
  def numDimsDataRangeKJI : Range = numDimsDataRange.reverse

  def innerDimsLocal : ListBuffer[IR_Expression] = numDimsDataRange.map(d => endIndices(d) - beginIndices(d) : IR_Expression).to[ListBuffer]

  def startIndexLocal : ListBuffer[IR_Expression] = numDimsDataRange.map(d => referenceOffset(d)).to[ListBuffer]

  def innerDimsGlobal : ListBuffer[IR_Expression] = if(Knowledge.domain_onlyRectangular) {
    numDimsDataRange.map(d => Knowledge.domain_rect_numFragsTotalAsVec(d) * innerDimsLocal(d) : IR_Expression).to[ListBuffer]
  } else { // TODO handling for other domain types
    Logger.error("Unimplemented!")
    numDimsDataRange.map(_ => IR_NullExpression : IR_Expression).to[ListBuffer]
  }

  def startIndexGlobal : ListBuffer[IR_Expression] = if(Knowledge.domain_onlyRectangular) {
    numDimsDataRange.map(d => innerDimsLocal(d) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsTotalAsVec(d)) : IR_Expression).to[ListBuffer]
  } else { // TODO handling for other domain types
    Logger.error("Unimplemented!")
    numDimsDataRange.map(_ => IR_NullExpression : IR_Expression).to[ListBuffer]
  }

  // determines if some layers (e.g. ghost/pad/...) are excluded for I/O operations or not
  def accessWithoutExclusion : IR_Expression = numDimsDataRange.map(d => innerDimsLocal(d) EqEq totalDimsLocal(d)).fold(IR_BooleanConstant(true))((a, b) => a AndAnd b)

  // describes in-memory access pattern of an array (linearized "distances" to next value in the same dimension)
  def imap : ListBuffer[IR_Expression] = numDimsDataRange.map(d => {
    if(d == 0)
      IR_IntegerConstant(1) // stride in x direction is "1"
    else
      (0 until d).map(dd => totalDimsLocal(dd)).reduce(_ * _) // product of total points from "previous dimensions"
  }).to[ListBuffer]

  def getAccess(index : IR_Index) : IR_Access = accessPattern.callAccessFunction(index)

  def getAddress(index : IR_Index) = IR_AddressOf(getAccess(index))

  private val negativeReferenceOffset : IR_ExpressionIndex = IR_ExpressionIndex(referenceOffset.indices.map(idx => IR_Negative(idx)) : _*)
  def getBaseAddress : IR_AddressOf = getAddress(negativeReferenceOffset)
  def getAddressReferenceOffset : IR_AddressOf = getAddress(IR_ConstIndex(numDimsDataRange.map(_ => 0) : _*))
}
