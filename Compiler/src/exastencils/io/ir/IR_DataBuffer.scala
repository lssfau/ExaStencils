package exastencils.io.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_FragmentIndex
import exastencils.field.ir.IR_Field
import exastencils.grid.ir.IR_Localization
import exastencils.layoutTransformation.ir.IR_GenericTransform
import exastencils.layoutTransformation.ir.IR_LayoutTransformationCollection
import exastencils.logger.Logger

object IR_DataBuffer {
  /* helper methods */
  // output data fragment-after-fragment -> additional dimensionality in this case
  def handleFragmentDimension(
      canonicalOrder : Boolean,
      accessBlockwise : Boolean,
      dims : ListBuffer[IR_Expression],
      fragmentDim : IR_Expression,
      orderKJI : Boolean = true) : ListBuffer[IR_Expression] = {

    if (!canonicalOrder) {
      val dimsPerFrag = if (accessBlockwise) { if (orderKJI) dims.drop(1) else dims.dropRight(1) } else dims
      if (orderKJI) fragmentDim +: dimsPerFrag else dimsPerFrag :+ fragmentDim
    } else {
      dims
    }
  }

  // determines if field layout was transformed
  def inLayoutTransformationCollection(field: IR_Field) : Boolean = IR_LayoutTransformationCollection.getOpt.isDefined &&
    IR_LayoutTransformationCollection.getOpt.get.trafoStmts
      .collect { case stmt : IR_GenericTransform => stmt } // only consider generic transformations
      .exists(_.fields.exists { case (name, lvl) => name == field.name && lvl == field.level }) // check if any trafo contains field

  // reduce the number of duplicate declarations in the target code for identical dimensionalities
  private val dimensionalityMap : mutable.HashMap[String, IR_VariableDeclaration] = mutable.HashMap()
  def declareDimensionality(dt : IR_Datatype, name : String, localization : IR_Localization, dims : Option[ListBuffer[IR_Expression]] = None) : IR_VariableDeclaration = {
    val declName = name + localization.name
    val baseDt = dt.resolveBaseDatatype
    val lookup = baseDt.prettyprint + declName + (if (dims.isDefined) dims.get.hashCode() else 0).toString
    // cast dims when used in initializer list, otherwise "Wnarrowing" warning
    val castDims = (dims getOrElse Nil) map {
      case vAcc : IR_VariableAccess if vAcc.datatype != baseDt        => IR_Cast(baseDt, vAcc)
      case iv : IR_InternalVariable if iv.resolveDatatype() != baseDt => IR_Cast(baseDt, iv)
      case expr : IR_Expression                                       => expr
    }
    dimensionalityMap.getOrElseUpdate(
      lookup,
      IR_VariableDeclaration(dt, IR_FileAccess.declareVariable(declName), if (dims.isDefined) Some(IR_InitializerList(castDims : _*)) else None)) // dims already specified in KJI order
  }
  def resetDimensionalityMap() : Unit = dimensionalityMap.clear()

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
      name = field.codeName,
      accessPattern = pattern getOrElse IR_RegularAccessPattern(IR_AccessFieldFunction(field, Duplicate(slot))),
      datasetName = dataset getOrElse IR_NullExpression,
      canonicalStorageLayout = canonicalOrder,
      layoutTransformationTarget = Some(field).filter(inLayoutTransformationCollection),
      accessBlockwise = false,
      isTemporaryBuffer = false
    )
  }

  // data buffer for matrix component access
  // used, for example, in non-AA case:
  //    - accepts a vf's associated (vector) field as parameter
  //    - treat position vector (defined as IR_MatrixDatatype(IR_RealDatatype, numDims, 1) ) as "numDims" separate fields
  def apply(
      matField : IR_Field,
      accessIndices : Option[ListBuffer[IR_Index]],
      dataset : Option[IR_Expression],
      indexRow : IR_Expression,
      indexCol : IR_Expression,
      canonicalOrder : Boolean) : IR_DataBuffer = {

    // access component
    val componentIndex = Some(IR_ExpressionIndex(indexRow, indexCol))

    val slot = 0
    val pattern = if (accessIndices.isDefined && accessIndices.get.size == 6)
      IR_SWEAccessPattern(IR_AccessFieldFunction(matField, slot, componentIndex), accessIndices)
    else
      IR_RegularAccessPattern(IR_AccessFieldFunction(matField, slot, componentIndex))

    new IR_DataBuffer(
      slot = slot,
      datatype = matField.resolveBaseDatatype, // vec -> numDims * scalar
      localization = matField.localization,
      referenceOffset = IR_ExpressionIndex(matField.referenceOffset.indices.slice(0, matField.layout.numDimsGrid)),
      beginIndices = (0 until matField.layout.numDimsGrid).map(d => matField.layout.defIdxById("DLB", d) : IR_Expression).to[ListBuffer],
      endIndices = (0 until matField.layout.numDimsGrid).map(d => matField.layout.defIdxById("DRE", d) : IR_Expression).to[ListBuffer],
      totalDimsLocal = (0 until matField.layout.numDimsGrid).map(d => matField.layout.defTotal(d) : IR_Expression).to[ListBuffer],
      numDimsGrid = matField.layout.numDimsGrid,
      numDimsData = matField.layout.numDimsGrid,
      domainIdx = matField.domain.index,
      name = matField.codeName,
      accessPattern = pattern,
      datasetName = dataset getOrElse IR_NullExpression,
      canonicalStorageLayout = canonicalOrder,
      layoutTransformationTarget = Some(matField).filter(inLayoutTransformationCollection),
      accessBlockwise = false,
      isTemporaryBuffer = false
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
      accessPattern = pattern getOrElse IR_RegularAccessPattern(IR_AccessTempBufferFunction(tmpBuf)),
      datasetName = dataset getOrElse IR_NullExpression,
      canonicalStorageLayout = false,
      layoutTransformationTarget = None,
      accessBlockwise = true, // currently only implemented as block-wise to reduce number of file accesses
      isTemporaryBuffer = true
    )
  }
}

/// IR_DataBuffer
// wrapper class for fields and temporary buffers
// provides data extents and information needed for I/O

case class IR_DataBuffer(
    var slot : IR_Expression, // slot for field accesses
    var datatype : IR_Datatype, // datatype of the buffer
    var localization : IR_Localization, // indicates where data is located
    var accessPattern : IR_AccessPattern, // describes the in-memory access pattern
    var referenceOffset : IR_ExpressionIndex, // offset from the lower corner of the buffer to the first reference point
    var beginIndices : ListBuffer[IR_Expression], // specifies where the portion of interest in the buffer begins in each dimension
    var endIndices : ListBuffer[IR_Expression], // specifies where the portion of interest in the buffer ends in each dimension
    var totalDimsLocal : ListBuffer[IR_Expression], // total data extents of the buffer; including excluded data (e.g. pad/ghost layers)
    var numDimsGrid : Int, // dimensionality of the grid
    var numDimsData : Int, // dimensionality of the stored data
    var domainIdx : Int, // ID of the (sub)domain the buffer lives on
    var name : String, // name of the buffer
    var datasetName : IR_Expression, // dataset name to be used in netCDF/HDF5 files
    var layoutTransformationTarget : Option[IR_Field], // original field with layout transform
    var canonicalStorageLayout : Boolean, // describes the data layout in the file
    var accessBlockwise : Boolean, // specifies if the data is stored per fragment (field/temp. buffers) or block (temp. buffers)
    var isTemporaryBuffer : Boolean // specified if underlying buffer is a temp. buffer
) {

  /* In this implementation, two data layouts are supported:
      1. Data is accessed in a canonical order (data is stored as if the whole global domain was written in one piece, i.e. as if is was never decomposed)
      2. Data is laid out in fragment-wise order (e.g. the data of fragment "1" is stored after fragment "0" linearly)
  */
  val canonicalOrder : Boolean = {
    if (canonicalStorageLayout && !Knowledge.domain_onlyRectangular) {
      Logger.warn(s"""Ignored canonical layout flag for IR_DataBuffer \"$name\": only applicable for \"domain_onlyRectangular\". Fragment-wise storage is used instead.""")
    }
    canonicalStorageLayout && Knowledge.domain_onlyRectangular
  }

  /*
  determine data extents:
    - computed from the local data dims that originate from the source that was passed to this wrapper
    - independent of the access pattern that was specified
    - specified in "KJI" order (slowest varying dim. at first index) or in "IJK" order (fastest varying dim first)
  */

  // declare array with a buffer's dimensionality
  def declareDimensionality(name : String, baseDatatype : IR_Datatype, dims : ListBuffer[IR_Expression]) : IR_VariableDeclaration = {
    IR_DataBuffer.declareDimensionality(IR_ArrayDatatype(baseDatatype, dims.length), name, localization, Some(dims))
  }

  // dimensions of the buffer's source (e.g. field)
  def numDimsDatatype : Int = numDimsData - numDimsGrid
  def numDimsGridRange : Range = 0 until numDimsGrid
  def numDimsDataRange : Range = 0 until numDimsData

  // determine number of dims of the final dataset written to file
  // may differ from the dims of the buffer's source
  // depends on file layout: canonical: identical to local num of dims, fragment-wise: additional "fragment dimension"
  def datasetDimsLocal : Int = if (canonicalOrder) startIndexLocalKJI.length else datasetDimsGlobal
  def datasetDimsGlobal : Int = globalDimsKJI.length

  def stride : ListBuffer[IR_Expression] = numDimsDataRange.map(_ => 1 : IR_Expression).to[ListBuffer]
  def strideKJI : ListBuffer[IR_Expression] = stride.reverse

  // temp. buffers: remove "fragment dimension"
  def innerDimsPerFrag : ListBuffer[IR_Expression] = if (accessBlockwise) innerDimsLocal.dropRight(1) else innerDimsLocal
  def totalDimsPerFrag : ListBuffer[IR_Expression] = if (accessBlockwise) totalDimsLocal.dropRight(1) else totalDimsLocal
  def startIndexPerFrag : ListBuffer[IR_Expression] = if (accessBlockwise) startIndexLocal.dropRight(1) else startIndexLocal

  def innerDimsLocal : ListBuffer[IR_Expression] = numDimsDataRange.map(d => endIndices(d) - beginIndices(d) : IR_Expression).to[ListBuffer]
  def innerDimsLocalKJI : ListBuffer[IR_Expression] = innerDimsLocal.reverse

  def startIndexLocal : ListBuffer[IR_Expression] = numDimsDataRange.map(d => beginIndices(d)).to[ListBuffer]
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

  // get start index for canonical layout
  def canonicalStartIndexGlobal(forIndex : Seq[IR_Expression]) : ListBuffer[IR_Expression] = {
    numDimsDataRange.map(d => innerDimsLocal(d) *
      (if (d < Knowledge.dimensionality) forIndex(d) Mod Knowledge.domain_rect_numFragsTotalAsVec(d) else 0) : IR_Expression).to[ListBuffer]
  }

  // get start index for fragment-wise layout
  def fragmentwiseStartIndexGlobal(forIndex : IR_Expression) : ListBuffer[IR_Expression] = {
    // buffer contains data for the whole block -> fragment count already contained in dimensionalities (index "0" in KJI order)
    (if (accessBlockwise) innerDimsLocal.dropRight(1) else innerDimsLocal).map(_ => 0 : IR_Expression) :+
      forIndex
  }

  def fragmentwiseStartOffset : IR_Expression = {
    (accessPattern.transformDataExtents(if (accessBlockwise) innerDimsLocal.dropRight(1) else innerDimsLocal, localization, orderKJI = false)
      :+ IR_IV_FragmentOffset(domainIdx)).reduce(_ * _)
  }

  def startIndexGlobal : ListBuffer[IR_Expression] = {
    if (canonicalOrder)
      canonicalStartIndexGlobal(numDimsGridRange.map(IR_IV_FragmentIndex(_)))
    else
      fragmentwiseStartIndexGlobal(IR_IV_FragmentOffset(domainIdx) + IR_LoopOverFragments.defIt)
  }
  def startIndexGlobalKJI : ListBuffer[IR_Expression] = startIndexGlobal.reverse

  // describes in-memory access pattern of an array (linearized "distances" to next value in the same dimension)
  def imap : ListBuffer[IR_Expression] = numDimsDataRange.map(d => {
    if (d == 0)
      IR_IntegerConstant(1) // stride in x direction is "1"
    else
      (0 until d).map(dd => totalDimsLocal(dd)).reduce(_ * _) // product of total points from "previous dimensions"
  }).to[ListBuffer]
  def imapKJI : ListBuffer[IR_Expression] = imap.reverse

  // determines if some layers (e.g. ghost/pad/...) are excluded for I/O operations or not
  def evalAccessWithoutExclusion : Boolean = numDimsDataRange.map(d => innerDimsLocalKJI(d) == totalDimsLocalKJI(d)).reduce(_ && _)
  def accessWithoutExclusion : IR_Expression = numDimsDataRange
    .map(d => innerDimsLocalKJI(d) EqEq totalDimsLocalKJI(d))
    .fold(IR_BooleanConstant(true))((a, b) => a AndAnd b)

  /* helper function to get access indices for multidim. datatypes */
  def getIndicesMultiDimDatatypes : Array[IR_Index] = {
    if (numDimsData > numDimsGrid) {
      datatype match {
        case mat : IR_MatrixDatatype =>
          Array.range(0, mat.sizeM).flatMap(rows =>
            Array.range(0, mat.sizeN).map(cols =>
              IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDimsGrid).indices :+ IR_IntegerConstant(rows) :+ IR_IntegerConstant(cols))))
        case _ : IR_ScalarDatatype   =>
          Array(IR_LoopOverDimensions.defIt(numDimsGrid))
        case _                       =>
          Logger.error("Unsupported higher dimensional datatype used for I/O interface.")
      }
    } else {
      Array(IR_LoopOverDimensions.defIt(numDimsGrid))
    }
  }

  def loopOverDims(condition : IR_Expression, accessStatements : IR_Statement*) : IR_LoopOverDimensions = {
    IR_LoopOverDimensions(numDimsGrid,
      accessPattern.transformExpressionIndexRange(
        IR_ExpressionIndex(numDimsGridRange.map(dim => beginIndices(dim) - Duplicate(referenceOffset(dim)) : IR_Expression).toArray),
        IR_ExpressionIndex(numDimsGridRange.map(dim => endIndices(dim) - Duplicate(referenceOffset(dim)) : IR_Expression).toArray)
      ),
      IR_IfCondition(condition,
        accessStatements.to[ListBuffer]))
  }

  /*
  forwarding to a buffer's access pattern instance:
    - get the actual data extents depending on the pattern
    - call the registered callback function
  */

  // helper function to handle accesses for access patterns and multidim. datatypes
  def handleAccesses : ListBuffer[ListBuffer[IR_Access]] = {
    accessPattern.accessesForPattern(getIndicesMultiDimDatatypes : _*) // per access from the pattern: access components of a high-dim datatype
  }

  // accessBlockwise : temp. buffer for a whole block is used -> fragment count already incorporated in local dims
  private val innerDimsFragKJI = if (accessBlockwise) innerDimsLocalKJI.tail else innerDimsLocalKJI
  private val innerDimsBlockKJI = if (accessBlockwise) innerDimsLocalKJI else IR_IV_NumValidFrags(domainIdx) +: innerDimsLocalKJI
  private val typicalByteSize = datatype.resolveBaseDatatype.typicalByteSize
  private def getTransformedExtents(dims : ListBuffer[IR_Expression]) : ListBuffer[IR_Expression] = accessPattern.transformDataExtents(dims, localization, orderKJI = true)
  def typicalByteSizeFrag : IR_Expression = getTransformedExtents(innerDimsFragKJI).reduce(_ * _) * typicalByteSize
  def typicalByteSizeBlock : IR_Expression = getTransformedExtents(innerDimsBlockKJI).reduce(_ * _) * typicalByteSize
  def typicalByteSizeLocal : IR_Expression = getTransformedExtents(innerDimsLocalKJI).reduce(_ * _) * typicalByteSize
  def typicalByteSizeGlobal : IR_Expression = getTransformedExtents(globalDimsKJI).reduce(_ * _) * typicalByteSize

  def getAccess(index : IR_Index) : IR_Access = accessPattern.callAccessFunction(index)

  def getAddress(access : IR_Access) = IR_AddressOf(access)
  def getAddress(index : IR_Index) = IR_AddressOf(getAccess(index))

  private val negativeReferenceOffset : IR_ExpressionIndex = IR_ExpressionIndex(referenceOffset.indices.map(idx => IR_Negative(idx)) : _*)
  def getBaseAccess : IR_Access = getAccess(negativeReferenceOffset)
  def getBaseAddress : IR_AddressOf = getAddress(getBaseAccess)

  def zeroIndex = IR_ConstIndex(numDimsDataRange.map(_ => 0) : _*)
  def getAccessReferenceOffset : IR_Access = getAccess(zeroIndex)
  def getAddressReferenceOffset : IR_AddressOf = getAddress(getAccessReferenceOffset)
}
