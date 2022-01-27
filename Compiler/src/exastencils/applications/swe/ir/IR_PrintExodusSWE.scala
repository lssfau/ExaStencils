package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_IV_FragmentInfo
import exastencils.logger.Logger
import exastencils.visualization.ir.netCDF.IR_PrintExodus

/// IR_PrintExodusSWE
// 2D only
// for a variable number of fragments per block

@deprecated
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
    nodePositionsBuf.zipWithIndex.map { case (tmpBuf, idx) => IR_DataBuffer(tmpBuf, IR_IV_ActiveSlot(someCellField), None, Some(datasetCoords(idx))) }
  }

  override def dataBufferConnectivity : IR_DataBuffer = IR_DataBuffer(connectivityBuf, IR_IV_ActiveSlot(someCellField), None, Some(datasetConnectivity))

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
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

    if (constsIncluded) dataBuffersConstant ++ allFields else allFields
  }

  override def statementsForCleanup : ListBuffer[IR_Statement] = ListBuffer()
}
