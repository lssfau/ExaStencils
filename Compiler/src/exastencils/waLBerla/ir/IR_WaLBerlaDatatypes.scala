package exastencils.waLBerla.ir

import exastencils.base.ir.IR_SpecialDatatype

object IR_WaLBerlaDatatypes {
  def WB_IBlock = IR_SpecialDatatype("IBlock")
  def WB_BlockDataID = IR_SpecialDatatype("BlockDataID")
  def WB_StructuredBlockStorage = IR_SpecialDatatype("StructuredBlockStorage")
  def WB_StructuredBlockForest = IR_SpecialDatatype("StructuredBlockForest")

  def WB_CommScheme(wbStencil : String) = IR_SpecialDatatype(s"blockforest::communication::UniformBufferedScheme<${wbStencil}>")

  def WB_FieldDatatype(field : IR_WaLBerlaField) =
    IR_SpecialDatatype(s"${field.waLBerlaFieldType} < ${field.resolveBaseDatatype.prettyprint}, ${field.gridDatatype.resolveFlattendSize} >")
}
