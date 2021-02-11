package exastencils.applications.swe.ir

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.grid.ir.IR_AtNode
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_IV_FragmentInfo
import exastencils.io.ir.IR_IV_TemporaryBuffer
import exastencils.logger.Logger
import exastencils.visualization.ir.IR_PrintExodus

/// IR_PrintExodusSWE
// 2D only
// for a variable number of fragments per block

case class IR_PrintExodusSWE(
    var filename : IR_Expression,
    level : Int,
    var resolveId : Int,
    var nodalFieldCollection : ListBuffer[IR_Field],
    var discFieldCollection : ListBuffer[ListBuffer[IR_Field]]
) extends IR_PrintExodus with IR_PrintVisualizationSWE {

  override def variableEntityType : IR_VariableAccess = EX_NODAL
  override def elementName : String = "tri"
  override def nodesPerElement : Int = 3

  override def statementsForPreparation : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()
    stmts ++= IR_IV_FragmentInfo.init(domainIndex, calculateFragOffset = true)
    stmts ++= setupNodePositions()
    stmts ++= setupConnectivity(global = true)
    // disc fields
    if (Knowledge.swe_nodalReductionPrint)
      stmts ++= setupReducedData
    else
      stmts ++= setupNonReducedDiscData

    stmts
  }

  /*
  in exodus, the individual datasets of each disc field component cannot be joined together as in xdmf
    - this causes the disc components to be written in an interleaved fashion
    - to reduce the number of interleaved accesses, the disc components are joined together in a buffer before writing
  */
  def discFieldBuffers : ListMap[String, IR_IV_TemporaryBuffer] = discFields.map { discField =>
    discField._1 -> IR_IV_TemporaryBuffer(discField._2.head.resolveBaseDatatype, IR_AtNode, discField._1, domainIndex, dimsPositionsFrag)
  }

  def setupNonReducedDiscData : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    // join disc components
    discFieldBuffers.values.foreach { tmpBuf =>
      val indexRangeCells = IR_ExpressionIndexRange(
        IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
        IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)))

      val numAccessesPerCell = nodeOffsets.length // for non-reduced SWE "6"
      val offset = IR_LoopOverFragments.defIt * dimsPositionsFrag.reduce(_ * _) + numAccessesPerCell * indexRangeCells.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))

      stmts += tmpBuf.allocateMemory
      stmts += IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
          IR_LoopOverDimensions(numDimsGrid, indexRangeCells,
            (0 until numAccessesPerCell).to[ListBuffer].map(idx => {
              IR_Assignment(
                tmpBuf.at(offset + idx),
                IR_FieldAccess(discFields(tmpBuf.name)(idx), IR_IV_ActiveSlot(someCellField), IR_LoopOverDimensions.defIt(numDimsGrid)))  : IR_Statement
            }))))
    }

    stmts
  }

  override def discFieldsToDatabuffers(discField : ListBuffer[IR_Field]) : ListBuffer[IR_DataBuffer] = {
    val fieldname = getBasenameDiscField(discField)
    val dataset = datasetFields(fieldnames.indexOf(fieldname))
    val buffer = if (Knowledge.swe_nodalReductionPrint) {
      IR_DataBuffer(discFieldsReduced(fieldname), IR_IV_ActiveSlot(someCellField), None, Some(dataset))
    } else {
      IR_DataBuffer(discFieldBuffers(fieldname), IR_IV_ActiveSlot(someCellField), None, Some(dataset))
    }

    ListBuffer(buffer)
  }

  override def dataBuffersNodePos : ListBuffer[IR_DataBuffer] = if (!gridPositionsCopied) {
    nodePosVecAsDataBuffers(accessIndices, Some(datasetCoords.map(s => s : IR_Expression)))
  } else {
    // no associated field for vf -> copy positions to buffer
    nodePositionsBuf.zipWithIndex.map { case(tmpBuf, idx) => IR_DataBuffer(tmpBuf, IR_IV_ActiveSlot(someCellField), None, Some(datasetCoords(idx))) }
  }

  override def dataBuffersConnectivity : IR_DataBuffer = IR_DataBuffer(connectivityBuf, IR_IV_ActiveSlot(someCellField), None, Some(datasetConnectivity))

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
    val constants = dataBuffersNodePos :+ dataBuffersConnectivity

    // bath is constant but cannot be reduced in this format since in Exodus fields are defined as record variables (i.e. bound to time)
    val allFields = fields.to[ListBuffer].zipWithIndex.flatMap { case ((_, fieldCollection), idx) =>
      // distinguish nodal and disc fields
      fieldCollection.length match {
        case 1 =>
          val nodalField = fieldCollection.head
          ListBuffer(IR_DataBuffer(nodalField, IR_IV_ActiveSlot(nodalField), includeGhosts = false, Some(nodalAccess(nodalField)), Some(datasetFields(idx)), canonicalOrder = false))
        case 6 =>
          discFieldsToDatabuffers(fieldCollection)
        case _ =>
          Logger.error("IR_PrintExodusSWE: Unknown field type; neither nodal nor disc field.")
      }
    }

    if (constsIncluded) constants ++ allFields else allFields
  }

  override def statementsForCleanup : ListBuffer[IR_Statement] = ListBuffer()
}
