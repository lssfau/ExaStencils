package exastencils.applications.ns.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_IV_FragmentInfo
import exastencils.visualization.ir.netCDF.IR_PrintExodus


/// IR_PrintExodusNNF
// 2D or 3D
// for a fixed number of fragments per block

@deprecated
case class IR_PrintExodusNNF(
    var filename : IR_Expression,
    level : Int,
    var resolveId : Int
) extends IR_PrintExodus with IR_PrintVisualizationNS {

  override def variableEntityType : IR_VariableAccess = EX_ELEM_BLOCK
  override def elementName : String = if (numDimsGrid > 2) "hex" else "quad"
  override def nodesPerElement : Int = connectivityForCell().length

  override def fieldnames : ListBuffer[String] = (0 until numDimsGrid).map(d => "vel" + ('X' + d).toChar.toString).to[ListBuffer] ++ ListBuffer("p", "rho", "mue", "gamma", "phi")

  override def statementsForPreparation : ListBuffer[IR_Statement] = {
    IR_IV_FragmentInfo.init(domainIndex) ++
      setupNodePositions() ++
      setupConnectivity(global = true) ++
      setupVelocityComponents
  }

  override def dataBuffersNodePos : ListBuffer[IR_DataBuffer] = nodePositionsBuf.zipWithIndex.map { case (buf, idx) =>
    IR_DataBuffer(buf, IR_IV_ActiveSlot(p), None, Some(datasetCoords(idx)))
  }

  override def dataBufferConnectivity : IR_DataBuffer = IR_DataBuffer(connectivityBuf, IR_IV_ActiveSlot(p), None, Some(datasetConnectivity))

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
    var fields = velocityComponentsAsVec.zipWithIndex.map { case (tmpBuf, d) =>
      IR_DataBuffer(tmpBuf, IR_IV_ActiveSlot(u), None, Some(datasetFields(d)))
    }.to[ListBuffer]
    fields += IR_DataBuffer(p, IR_IV_ActiveSlot(p), includeGhosts = false, None, Some(datasetFields(numDimsGrid)), canonicalOrder = false)
    fields += IR_DataBuffer(rho, IR_IV_ActiveSlot(rho), includeGhosts = false, None, Some(datasetFields(numDimsGrid + 1)), canonicalOrder = false)
    fields += IR_DataBuffer(mue, IR_IV_ActiveSlot(mue), includeGhosts = false, None, Some(datasetFields(numDimsGrid + 2)), canonicalOrder = false)
    fields += IR_DataBuffer(gamma, IR_IV_ActiveSlot(gamma), includeGhosts = false, None, Some(datasetFields(numDimsGrid + 3)), canonicalOrder = false)
    fields += IR_DataBuffer(phi, IR_IV_ActiveSlot(phi), includeGhosts = false, None, Some(datasetFields(numDimsGrid + 4)), canonicalOrder = false)

    if (constsIncluded) dataBuffersConstant ++ fields else fields
  }

  override def statementsForCleanup : ListBuffer[IR_Statement] = ListBuffer()
}
