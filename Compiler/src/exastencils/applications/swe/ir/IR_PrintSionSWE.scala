package exastencils.applications.swe.ir
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expandable
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Index
import exastencils.base.ir.IR_NullExpression
import exastencils.base.ir.IR_Statement
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.OutputType
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.io.ir.IR_AccessPattern
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_FileAccess_SionLib
import exastencils.logger.Logger

/// IR_PrintSionSWE
// 2D only
// for a variable number of fragments per block
// mainly used for comparison since produced ".sion" files cannot be visualized directly (need to be converted first -> post-process)

case class IR_PrintSionSWE(
    var filename : IR_Expression,
    level : Int,
    var resolveId : Int,
    var nodalFieldCollection : ListBuffer[IR_Field],
    var discFieldCollection : ListBuffer[ListBuffer[IR_Field]]
) extends IR_Statement with IR_Expandable with IR_PrintVisualizationSWE {

  override def ioInterface : String = "sion"

  override def discFieldsToDatabuffers(discField : ListBuffer[IR_Field]) : ListBuffer[IR_DataBuffer] = {
    // source for data buffer is dependent on reduction mode: temp buffer or field
    val fieldname = getBasenameDiscField(discField)
    if (Knowledge.swe_nodalReductionPrint) {
      ListBuffer(IR_DataBuffer(discFieldsReduced(fieldname), IR_IV_ActiveSlot(someCellField), None, None))
    } else {
      discField.map { field =>
        val idxRange = 0 until field.layout.numDimsData
        val slot = IR_IV_ActiveSlot(field)
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
          accessPattern = IR_AccessPattern((idx : IR_Index) => IR_FieldAccess(field, slot, idx.toExpressionIndex)),
          datasetName = IR_NullExpression,
          name = field.name,
          canonicalStorageLayout = false,
          accessBlockwise = false
        )
      }
    }
  }

  def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
    // access pattern dependent on reduction mode for blockstructured meshes
    val accessIndices : Option[ListBuffer[IR_Index]] = if (Knowledge.swe_nodalReductionPrint)
      None
    else
      Some(nodeOffsets.map(_.toExpressionIndex))
    def nodalAccess(field : IR_Field) = IR_AccessPattern((idx : IR_Index) => IR_FieldAccess(field, IR_IV_ActiveSlot(field), idx.toExpressionIndex), accessIndices)

    // bath is constant and can be reduced -> move to constants if exists
    val bath = nodalFields.get("bath")
    var constants : ListBuffer[IR_DataBuffer] = ListBuffer()
    if (gridPositionsCopied) {
      // copy positions to buffer if no associated field for vf
      constants ++= nodePositionsBuf.map(IR_DataBuffer(_, IR_IV_ActiveSlot(someCellField), None, None))
    } else {
      // use vf's associated field directly
      constants ++= nodePosVecAsDataBuffers(accessIndices, None)
    }
    constants += IR_DataBuffer(connectivityBuf, IR_IV_ActiveSlot(someCellField), None, None)
    if (bath.isDefined) {
      constants += IR_DataBuffer(bath.get, IR_IV_ActiveSlot(bath.get), includeGhosts = false, Some(nodalAccess(bath.get)), None, canonicalOrder = false)
    }

    // non-constant fields
    val nonConstFields = fields.filterKeys(_ != "bath").values.flatMap { fieldCollection =>
      fieldCollection.length match {
        case 1 =>
          val nodeField = fieldCollection.head
          ListBuffer(IR_DataBuffer(nodeField, IR_IV_ActiveSlot(nodeField), includeGhosts = false, Some(nodalAccess(nodeField)), None, canonicalOrder = false))
        case 6 =>
          discFieldsToDatabuffers(fieldCollection)
        case _ =>
          Logger.error("IR_PrintSionSWE: Unknown field type; neither nodal nor disc field.")
      }
    }

    if (constsIncluded) constants ++ nonConstFields else nonConstFields.to[ListBuffer]
  }

  def writeData(constsIncluded : Boolean) : ListBuffer[IR_Statement] = ListBuffer(ioHandler(constsIncluded))

  def ioHandler(constsIncluded : Boolean) = IR_FileAccess_SionLib(filename, dataBuffers(constsIncluded), writeAccess = true, interleavedAccHighDimDt = false, condition = true)

  override def expand() : OutputType ={
   var statements : ListBuffer[IR_Statement] = ListBuffer()

    // init temp buffers
    if (gridPositionsCopied)
      statements ++= setupNodePositions(copyNodePositions = true)
    statements ++= setupConnectivity(global = true)
    if (Knowledge.swe_nodalReductionPrint) {
      statements ++= setupReducedData
    }

    statements += IR_IfCondition(IR_ConstantsWrittenToFile().isEmpty,
      /* true: write constants to file and save filename to reference later */
      writeData(constsIncluded = true) :+ IR_ConstantsWrittenToFile().setFilename(basename(noPath = true), Some(ext)),
      /* false: write field data and reference constants from saved filename */
      writeData(constsIncluded = false))

    statements
  }

}
