package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Index
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.config.Knowledge
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.io.ir.IR_AccessPattern
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_IV_FragmentInfo
import exastencils.visualization.ir.IR_PrintExodus

/// IR_PrintExodusSWE
// 2D only
// for a variable number of fragments per block

case class IR_PrintExodusSWE(
    var filename : IR_Expression,
    level : Int,
    var resolveId : Int,
) extends IR_PrintExodus with IR_PrintVisualizationSWE {

  override def variableEntityType : IR_VariableAccess = EX_NODAL
  override def elementName : String = "tri"
  override def nodesPerElement : Int = 3

  override def statementsForPreparation : ListBuffer[IR_Statement] = {
    IR_IV_FragmentInfo.init(domainIndex, calculateFragOffset = true) ++
      setupNodePositions ++
      setupConnectivity(global = true) ++
      (if (Knowledge.swe_nodalReductionPrint) setupReducedData else ListBuffer())
  }

  override def discFieldsToDatabuffers(discField : ListBuffer[IR_Field]) : ListBuffer[IR_DataBuffer] = {
    val fieldname = getBasenameDiscField(discField)
    val dataset = datasetFields(fieldnames.indexOf(fieldname))
    if (Knowledge.swe_nodalReductionPrint) {
      ListBuffer(
        IR_DataBuffer(discFieldsReduced(fieldname), IR_IV_ActiveSlot(someCellField), None, Some(dataset))
      )
    } else {
      // TODO
      ListBuffer()
    }
  }

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
    // access pattern dependent on reduction mode for blockstructured meshes
    val accessIndices : Option[ListBuffer[IR_Index]]= if (Knowledge.swe_nodalReductionPrint)
      None
    else
      Some(nodeOffsets.map(_.toExpressionIndex))
    def nodalAccess(field : IR_Field) = IR_AccessPattern((idx : IR_Index) => IR_FieldAccess(field, IR_IV_ActiveSlot(field), idx.toExpressionIndex), accessIndices)

    val constants = nodePosVecAsDataBuffers(accessIndices, datasetCoords.map(s => s : IR_Expression)) :+
      IR_DataBuffer(connectivityBuf, IR_IV_ActiveSlot(someCellField), None, Some(datasetConnectivity))
    // bath is constant but cannot be reduced in this format since in Exodus fields are defined as record variables (i.e. bound to time)
    val allFields = fields.values.to[ListBuffer].zipWithIndex.flatMap { case (fieldCollection, idx) =>
      if (fieldCollection.length == 1) {
        // nodal field
        val nodalField = fieldCollection.head
        ListBuffer(
          IR_DataBuffer(nodalField, IR_IV_ActiveSlot(nodalField), includeGhosts = false, Some(nodalAccess(nodalField)), Some(datasetFields(idx)), canonicalOrder = false)
        )
      } else {
        // disc field
        discFieldsToDatabuffers(fieldCollection)
      }
    }

    if (constsIncluded) constants ++ allFields else allFields
  }

  override def statementsForCleanup : ListBuffer[IR_Statement] = ListBuffer()
}
