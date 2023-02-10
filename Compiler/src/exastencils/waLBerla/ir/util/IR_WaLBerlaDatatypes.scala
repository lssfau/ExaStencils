package exastencils.waLBerla.ir.util

import exastencils.base.ir.IR_SpecialDatatype
import exastencils.waLBerla.ir.field.IR_WaLBerlaField

object IR_WaLBerlaDatatypes {
  def WB_IBlock = IR_SpecialDatatype("IBlock")
  def WB_Block = IR_SpecialDatatype("Block")
  def WB_BlockDataID = IR_SpecialDatatype("BlockDataID")
  def WB_StructuredBlockStorage = IR_SpecialDatatype("StructuredBlockStorage")
  def WB_StructuredBlockForest = IR_SpecialDatatype("StructuredBlockForest")

  def WB_CommScheme(onGPU : Boolean) =
    if (onGPU)
      IR_SpecialDatatype(s"cuda::communication::UniformGPUScheme< $WB_StencilTemplate >")
    else
      IR_SpecialDatatype(s"blockforest::communication::UniformBufferedScheme<$WB_StencilTemplate>")

  def WB_StencilTemplate = "Stencil_T"

  def WB_FieldDatatype(field : IR_WaLBerlaField, onGPU : Boolean) =
    if (onGPU)
      IR_SpecialDatatype(s"cuda::GPUField< real_t >")
    else
      IR_SpecialDatatype(s"${ "GhostLayerField" } < ${ field.resolveBaseDatatype.prettyprint }, ${ field.gridDatatype.resolveFlattendSize } >")
}
