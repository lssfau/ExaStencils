package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ImplicitConversion._
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

  // accepts a field as input and retrieves dims, localization, ... from it
  def apply(field : IR_Field, slot : IR_Expression, includeGhosts : Boolean, pattern : Option[IR_AccessPattern]) : IR_DataBuffer = {
    val idxRange = (0 until field.layout.numDimsData).reverse // KJI order
    new IR_DataBuffer(
      slot = slot,
      datatype = field.layout.datatype,
      localization = field.localization,
      accessPattern = pattern.getOrElse(IR_AccessPattern((idx : IR_Expression) => IR_FieldAccess(field, Duplicate(slot), IR_ExpressionIndex(idx)))),
      referenceOffset = field.referenceOffset,
      beginIndices = idxRange.map(d => field.layout.defIdxById(if(includeGhosts) "GLB" else "DLB", d) : IR_Expression).to[ListBuffer],
      endIndices = idxRange.map(d => field.layout.defIdxById(if(includeGhosts) "GRE" else "DRE", d) : IR_Expression).to[ListBuffer],
      totalDimsLocal = idxRange.map(d => field.layout.defTotal(d) : IR_Expression).to[ListBuffer],
      numDimsGrid = field.layout.numDimsGrid,
      numDimsData = field.layout.numDimsData,
      domainIdx = field.domain.index,
      name = field.name,
      accessBlockwise = false,
      isDiscField = false
    )
  }

  // assumes the temporary buffer only contains the values of interest (e.g. no padding/ghost layers to exclude)
  def apply(tmpBuf : IR_IV_TemporaryBuffer, slot : IR_Expression, localization: IR_Localization, pattern : Option[IR_AccessPattern]) : IR_DataBuffer = new IR_DataBuffer(
    slot = slot,
    datatype = tmpBuf.resolveDatatype(),
    localization = localization,
    accessPattern = pattern.getOrElse(IR_AccessPattern((idx : IR_Expression) => tmpBuf.resolveAccess(index = idx))),
    referenceOffset = IR_ExpressionIndex(Array.fill(tmpBuf.numDims)(0)),
    beginIndices = ListBuffer.fill(tmpBuf.numDims)(0),
    endIndices = tmpBuf.dims,
    totalDimsLocal = tmpBuf.dims,
    numDimsGrid = tmpBuf.dims.length,
    numDimsData = tmpBuf.dims.length,
    domainIdx = IR_DomainCollection.getByIdentifier("global").get.index,
    name = tmpBuf.name,
    accessBlockwise = true,
    isDiscField = false
  )

  // special case for SWE (e.g. etaDiscLower0, ...)
  def apply(discField : ListBuffer[IR_Field], slot : IR_Expression, pattern : Option[IR_AccessPattern]) : IR_DataBuffer = {
    val field = discField.head
    val idxRange = (0 until field.layout.numDimsData).reverse // KJI
    new IR_DataBuffer(
      slot = slot,
      datatype = field.resolveBaseDatatype,
      localization = field.layout.localization,
      accessPattern = pattern.getOrElse(IR_AccessPattern((idx : IR_Expression) => IR_FieldAccess(field, Duplicate(slot), IR_ExpressionIndex(idx)))),
      referenceOffset = field.referenceOffset,
      beginIndices = idxRange.map(d => field.layout.defIdxById("IB", d) : IR_Expression).to[ListBuffer],
      endIndices = idxRange.map(d => field.layout.defIdxById("IE", d) : IR_Expression).to[ListBuffer],
      totalDimsLocal = idxRange.map(d => field.layout.defTotal(d) : IR_Expression).to[ListBuffer],
      numDimsGrid = field.layout.numDimsGrid,
      numDimsData = field.layout.numDimsData,
      domainIdx = field.domain.index,
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
    var accessBlockwise : Boolean, // TODO do we actually need to specify this explicitly?
    var isDiscField : Boolean // TODO
) {

  def stride : Array[IR_Expression] = accessPattern.stridePerDimension.getOrElse(numDimsDataRange.map(_ => 1 : IR_Expression).toArray)

  def numDimsDataRange : Range = (0 until numDimsData).reverse // KJI order

  def innerDimsLocal : Array[IR_Expression] = numDimsDataRange.map(d => endIndices(d) - beginIndices(d) : IR_Expression).toArray

  def startIndexGlobal : Array[IR_Expression] = numDimsDataRange.map(d => referenceOffset(d)).toArray

  def innerDimsGlobal : Array[IR_Expression] = if(Knowledge.domain_onlyRectangular) {
    numDimsDataRange.map(d => Knowledge.domain_rect_numFragsTotalAsVec(d) * innerDimsLocal(d)).toArray
  } else { // TODO handling for other domain types
    Logger.error("Unimplemented!")
    numDimsDataRange.map(_ => IR_NullExpression).toArray
  }

  def startIdxGlobal : Array[IR_Expression] = if(Knowledge.domain_onlyRectangular) {
    numDimsDataRange.map(d => innerDimsLocal(d) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsTotalAsVec(d))).toArray
  } else { // TODO handling for other domain types
    Logger.error("Unimplemented!")
    numDimsDataRange.map(_ => IR_NullExpression).toArray
  }

  def resolveAccess(index : IR_ExpressionIndex) : IR_Access = accessPattern.accessFunction(index)

  //def basePtr(firstInnerPoint : Boolean) : IR_AddressOf
}
