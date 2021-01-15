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
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_FragmentIndex
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.grid.ir.IR_Localization
import exastencils.logger.Logger

object IR_DataBuffer {
  /* helper methods */
  // output data fragment-after-fragment -> additional dimensionality in this case
  def handleFragmentDimension(buf : IR_DataBuffer, dims : ListBuffer[IR_Expression], fragmentDim : IR_Expression, orderKJI : Boolean = true) : ListBuffer[IR_Expression] = {
    if (!buf.canonicalOrder) {
      val dimsPerFrag = if (buf.accessBlockwise) dims.drop(1) else dims
      if (orderKJI) fragmentDim +: dimsPerFrag else dimsPerFrag :+ fragmentDim
    } else {
      dims
    }
  }

  /* apply methods */
  // accepts a field as input and retrieves dims, localization, etc. from it
  def apply(
      field : IR_Field,
      slot : IR_Expression,
      includeGhosts : Boolean,
      pattern : Option[IR_AccessPattern],
      dataset : Option[IR_Expression],
      canonicalOrder : Boolean) : IR_DataBuffer = {

    new IR_DataBuffer(
      slot = slot,
      datatype = field.gridDatatype,
      localization = field.localization,
      referenceOffset = field.referenceOffset,
      beginIndices = (0 until field.layout.numDimsData).map(d => field.layout.defIdxById(if (includeGhosts) "GLB" else "DLB", d) : IR_Expression).to[ListBuffer],
      endIndices = (0 until field.layout.numDimsData).map(d => field.layout.defIdxById(if (includeGhosts) "GRE" else "DRE", d) : IR_Expression).to[ListBuffer],
      totalDimsLocal = (0 until field.layout.numDimsData).map(d => field.layout.defTotal(d) : IR_Expression).to[ListBuffer],
      numDimsGrid = field.layout.numDimsGrid,
      numDimsData = field.layout.numDimsData,
      domainIdx = field.domain.index,
      name = field.name,
      accessPattern = pattern getOrElse IR_AccessPattern((idx : IR_Index) => IR_FieldAccess(field, Duplicate(slot), idx.toExpressionIndex)),
      datasetName = dataset getOrElse IR_NullExpression,
      canonicalStorageLayout = canonicalOrder,
      accessBlockwise = false,
      isDiscField = false
    )
  }

  // non-AA case: accepts a vf's associated field
  def apply(
      vfAssocField : IR_Field,
      accessIndices: ListBuffer[IR_Index],
      dataset: Option[IR_Expression],
      dim : Int) : IR_DataBuffer = {

    if (Knowledge.grid_isAxisAligned) {
      Logger.error("Trying to access associated field for subclass of \"IR_VirtualFieldWithVec\"; Not applicable for AA grids.")
    }

    // treat position vector (defined as IR_MatrixDatatype(IR_RealDatatype, numDims, 1) ) as "numDims" separate fields
    def highDimIndex(idx : IR_Index) = IR_ExpressionIndex(idx.toExpressionIndex.indices :+ (dim : IR_Expression) :+ (0 : IR_Expression)) // access vector component for "dim"
    new IR_DataBuffer(
      slot = 0,
      datatype = vfAssocField.resolveBaseDatatype, // vec -> numDims * scalar
      localization = vfAssocField.localization,
      referenceOffset = IR_ExpressionIndex(vfAssocField.referenceOffset.indices.slice(0, vfAssocField.layout.numDimsGrid)),
      beginIndices = (0 until vfAssocField.layout.numDimsGrid).map(d => vfAssocField.layout.defIdxById("DLB", d) : IR_Expression).to[ListBuffer],
      endIndices = (0 until vfAssocField.layout.numDimsGrid).map(d => vfAssocField.layout.defIdxById("DRE", d) : IR_Expression).to[ListBuffer],
      totalDimsLocal = (0 until vfAssocField.layout.numDimsGrid).map(d => vfAssocField.layout.defTotal(d) : IR_Expression).to[ListBuffer],
      numDimsGrid = vfAssocField.layout.numDimsGrid,
      numDimsData = vfAssocField.layout.numDimsGrid,
      domainIdx = vfAssocField.domain.index,
      name = vfAssocField.name,
      accessPattern = IR_AccessPattern((idx : IR_Index) => IR_FieldAccess(vfAssocField, 0, highDimIndex(idx)), accessIndices),
      datasetName = dataset getOrElse IR_NullExpression,
      canonicalStorageLayout = false,
      accessBlockwise = false,
      isDiscField = false
    )
  }

  // assumes the temporary buffer contains only the values of interest (e.g. no padding/ghost layers to exclude)
  def apply(
      tmpBuf : IR_IV_TemporaryBuffer,
      slot : IR_Expression,
      pattern : Option[IR_AccessPattern],
      dataset : Option[IR_Expression]) : IR_DataBuffer = {

    new IR_DataBuffer(
      slot = slot,
      datatype = tmpBuf.resolveDatatype(),
      localization = tmpBuf.localization,
      referenceOffset = tmpBuf.referenceOffset,
      beginIndices = tmpBuf.beginIndices,
      endIndices = tmpBuf.endIndices,
      totalDimsLocal = tmpBuf.totalDimsLocal,
      numDimsGrid = tmpBuf.numDims,
      numDimsData = tmpBuf.numDims,
      domainIdx = tmpBuf.domainIdx,
      name = tmpBuf.name,
      accessPattern = pattern getOrElse IR_AccessPattern((idx : IR_Index) => tmpBuf.at(idx)),
      datasetName = dataset getOrElse IR_NullExpression,
      canonicalStorageLayout = false,
      accessBlockwise = true,
      isDiscField = false
    )
  }
}

// TODO comments for params
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
    var canonicalStorageLayout : Boolean,
    var accessBlockwise : Boolean,
    var isDiscField : Boolean
) {

  /* In this implementation, two data layouts are supported:
      1. Data is accessed in a canonical order (data is stored as if the whole was written in one piece, i.e. as if the domain was never decomposed)
      2. Data is laid out in fragment-wise order (e.g. the data of fragment "1" is stored after fragment "0" linearly)
  */
  val canonicalOrder : Boolean = canonicalStorageLayout && Knowledge.domain_onlyRectangular

  def numDimsDatatype : Int = numDimsData - numDimsGrid
  def numDimsGridRange : Range = 0 until numDimsGrid
  def numDimsDataRange : Range = 0 until numDimsData

  def stride : ListBuffer[IR_Expression] = accessPattern.stridePerDimension.getOrElse(numDimsDataRange.map(_ => 1 : IR_Expression).to[ListBuffer])
  def strideKJI : ListBuffer[IR_Expression] = stride.reverse

  def innerDimsLocal: ListBuffer[IR_Expression] = numDimsDataRange.map(d => endIndices(d) - beginIndices(d) : IR_Expression).to[ListBuffer]
  def innerDimsLocalKJI : ListBuffer[IR_Expression] = innerDimsLocal.reverse

  def startIndexLocal: ListBuffer[IR_Expression] = numDimsDataRange.map(d => referenceOffset(d)).to[ListBuffer]
  def startIndexLocalKJI : ListBuffer[IR_Expression] = startIndexLocal.reverse

  def totalDimsLocalKJI : ListBuffer[IR_Expression] = totalDimsLocal.reverse

  def globalDims : ListBuffer[IR_Expression] = {
    if (canonicalOrder) {
      numDimsDataRange.map(d => innerDimsLocal(d) *
      (if (d < Knowledge.dimensionality) Knowledge.domain_rect_numFragsTotalAsVec(d) else 1) : IR_Expression).to[ListBuffer]
    } else {
      // buffer contains data for the whole block -> fragment count already contained in dimensionalities (index "0" in KJI order)
      (if (accessBlockwise) innerDimsLocal.dropRight(1) else innerDimsLocal) :+
        IR_IV_TotalNumFrags(domainIdx)
    }
  }
  def globalDimsKJI : ListBuffer[IR_Expression] = globalDims.reverse

  def canonicalStartIndexGlobal(forIndex : Seq[IR_Expression]) : ListBuffer[IR_Expression] = {
    numDimsDataRange.map(d => innerDimsLocal(d) *
      (if (d < Knowledge.dimensionality) forIndex(d) Mod Knowledge.domain_rect_numFragsTotalAsVec(d) else 0) : IR_Expression).to[ListBuffer]
  }

  def fragmentwiseStartIndexGlobal(forIndex : IR_Expression) : ListBuffer[IR_Expression] = {
    // buffer contains data for the whole block -> fragment count already contained in dimensionalities (index "0" in KJI order)
    (if (accessBlockwise) innerDimsLocal.dropRight(1) else innerDimsLocal).map(_ => 0 : IR_Expression) :+
      forIndex
  }

  def startIndexGlobal : ListBuffer[IR_Expression] = {
    if (canonicalOrder) {
      canonicalStartIndexGlobal(numDimsGridRange.map(IR_IV_FragmentIndex(_)))
    } else {
      fragmentwiseStartIndexGlobal(IR_IV_FragmentOffset(domainIdx) + IR_LoopOverFragments.defIt)
    }
  }
  def startIndexGlobalKJI : ListBuffer[IR_Expression] = startIndexGlobal.reverse

  // describes in-memory access pattern of an array (linearized "distances" to next value in the same dimension)
  // TODO handle "fragment dimension" when non-canonical order is used
  def imapKJI : ListBuffer[IR_Expression] = numDimsDataRange.map(d => {
    if (d == 0)
      IR_IntegerConstant(1) // stride in x direction is "1"
    else
      (0 until d).map(dd => totalDimsLocal(dd)).reduce(_ * _) // product of total points from "previous dimensions"
  }).to[ListBuffer].reverse

  // determines if some layers (e.g. ghost/pad/...) are excluded for I/O operations or not
  def accessWithoutExclusion : IR_Expression = numDimsDataRange
    .map(d => innerDimsLocalKJI(d) EqEq totalDimsLocalKJI(d))
    .fold(IR_BooleanConstant(true))((a, b) => a AndAnd b)

  // accessBlockwise : temp. buffer for a whole block is used -> fragment count already incorporated in local dims
  private val innerDimsFragKJI = if (accessBlockwise) innerDimsLocalKJI.tail else innerDimsLocalKJI
  private val innerDimsBlockKJI = if (accessBlockwise) innerDimsLocalKJI else IR_IV_NumValidFrags(domainIdx) +: innerDimsLocalKJI
  def typicalByteSizeFrag : IR_Expression = innerDimsFragKJI.reduce(_ * _) * datatype.resolveBaseDatatype.typicalByteSize
  def typicalByteSizeBlock : IR_Expression = innerDimsBlockKJI.reduce(_ * _) * datatype.resolveBaseDatatype.typicalByteSize
  def typicalByteSizeLocal : IR_Expression = innerDimsLocalKJI.reduce(_ * _) * datatype.resolveBaseDatatype.typicalByteSize
  def typicalByteSizeGlobal : IR_Expression = globalDimsKJI.reduce(_ * _) * datatype.resolveBaseDatatype.typicalByteSize

  def getAccess(index : IR_Index) : IR_Access = accessPattern.callAccessFunction(index)

  def getAddress(index : IR_Index) = IR_AddressOf(getAccess(index))

  private val negativeReferenceOffset : IR_ExpressionIndex = IR_ExpressionIndex(referenceOffset.indices.map(idx => IR_Negative(idx)) : _*)
  def getBaseAddress : IR_AddressOf = getAddress(negativeReferenceOffset)
  def getAddressReferenceOffset : IR_AddressOf = getAddress(IR_ConstIndex(numDimsDataRange.map(_ => 0) : _*))
}
