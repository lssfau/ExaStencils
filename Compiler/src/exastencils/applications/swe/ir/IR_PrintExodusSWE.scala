package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_Index
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_VariableAccess
import exastencils.config.Knowledge
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.io.ir.IR_AccessPattern
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_IV_FragmentInfo
import exastencils.visualization.ir.IR_PrintExodus
import exastencils.base.ir.IR_ImplicitConversion._

/// IR_PrintExodusSWE
// 2D only
// for a variable number of fragments per block

case class IR_PrintExodusSWE(
    var filename : IR_Expression,
    level : Int) extends IR_PrintExodus with IR_PrintVisualizationSWE {

  override def variableEntityType : IR_VariableAccess = EX_NODAL
  override def elementName : String = "tri"
  override def nodesPerElement : Int = 3

  override def statementsForPreparation : ListBuffer[IR_Statement] = {
    IR_IV_FragmentInfo.init(domainIndex, calculateFragOffset = true) ++
      setupNodePositions ++
      setupConnectivity(global = true) ++
      (if (Knowledge.swe_nodalReductionPrint) setupReducedData else ListBuffer())
  }

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
    // access pattern dependent on reduction mode for blockstructured meshes
    val accessIndices : ListBuffer[IR_Index] = if (Knowledge.swe_nodalReductionPrint)
      ListBuffer(IR_ConstIndex(Array.fill(numDimsGrid)(0)))
    else
      nodeOffsets.map(_.toExpressionIndex)
    val bathAccess = IR_AccessPattern((idx : IR_Index) => IR_FieldAccess(bath, IR_IV_ActiveSlot(bath), idx.toExpressionIndex), accessIndices)

    val constants = nodePosVecAsDataBuffers(accessIndices, datasetCoords.map(s => IR_StringConstant(s))) :+
      IR_DataBuffer(connectivityBuf, IR_IV_ActiveSlot(someCellField), None, Some(IR_StringConstant(datasetConnectivity)))
    // bath is constant but cannot be reduced in this format since in Exodus fields are defined as record variables (i.e. bound to time)
    val fields = IR_DataBuffer(bath, IR_IV_ActiveSlot(bath), includeGhosts = false, Some(bathAccess), Some(IR_StringConstant(datasetFields.head)), canonicalOrder = false) +:
      datasetFields.tail.zipWithIndex.map { case (ds, i) => discFieldsAsDataBuffers(discFields(i), IR_StringConstant(ds))}

    if (constsIncluded) constants ++ fields else fields
  }

  override def statementsForCleanup : ListBuffer[IR_Statement] = cleanupReducedData
}
