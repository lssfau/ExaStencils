package exastencils.waLBerla.ir.util

import exastencils.base.ir.IR_SpecialDatatype
import exastencils.config.Knowledge
import exastencils.logger.Logger
import exastencils.waLBerla.ir.field.IR_WaLBerlaField

object IR_WaLBerlaDatatypes {

  // custom typedefs

  def WB_RealType = IR_SpecialDatatype("real_t")
  def WB_UintType = IR_SpecialDatatype("uint_t")

  // data structures

  def WB_IBlock = IR_SpecialDatatype("IBlock")
  def WB_Block = IR_SpecialDatatype("Block")
  def WB_SetupBlock = IR_SpecialDatatype("SetupBlock")
  def WB_BlockID = IR_SpecialDatatype("BlockID")
  def WB_BlockDataID = IR_SpecialDatatype("BlockDataID")
  def WB_StructuredBlockStorage = IR_SpecialDatatype("StructuredBlockStorage")
  def WB_StructuredBlockForest = IR_SpecialDatatype("StructuredBlockForest")
  def WB_SetupBlockForest = IR_SpecialDatatype("SetupBlockForest")

  def WB_CommScheme(onGPU : Boolean) = {
    if (!Knowledge.waLBerla_useRefinement) {
      // uniform comm scheme
      if (onGPU)
        IR_SpecialDatatype(s"gpu::communication::UniformGPUScheme< $WB_StencilTemplate >")
      else
        IR_SpecialDatatype(s"blockforest::communication::UniformBufferedScheme<$WB_StencilTemplate>")
    } else {
      // non-uniform comm scheme
      if (onGPU)
        Logger.error("Non-uniform comm schemes for CUDA are not natively implemented in waLBerla.")
      else
        IR_SpecialDatatype(s"blockforest::communication::NonUniformBufferedScheme<$WB_StencilTemplate>")
    }
  }

  def WB_StencilTemplate = "Stencil_T"

  def WB_FieldDatatype(field : IR_WaLBerlaField, onGPU : Boolean) =
    if (onGPU)
      IR_SpecialDatatype(s"gpu::GPUField< ${WB_RealType.typeName} >")
    else
      IR_SpecialDatatype(s"${ "GhostLayerField" } < ${ field.resolveBaseDatatype.prettyprint }, ${ field.gridDatatype.resolveFlattendSize } >")
}
