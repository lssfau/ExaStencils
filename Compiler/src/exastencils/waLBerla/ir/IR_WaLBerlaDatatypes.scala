package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.field.ir.IR_Field

object IR_WaLBerlaDatatypes {
  def WB_IBlock = IR_SpecialDatatype("IBlock")
  def WB_BlockDataID = IR_SpecialDatatype("BlockDataID")
  def WB_StructuredBlockStorage = IR_SpecialDatatype("StructuredBlockStorage")

  def WB_FieldDatatype(field : IR_WaLBerlaField) = IR_SpecialDatatype(s"Field< ${field.resolveBaseDatatype.prettyprint}, ${field.gridDatatype.resolveFlattendSize} >")
  def WB_FieldDatatype(field : IR_Field) = IR_SpecialDatatype(s"Field< ${field.resolveBaseDatatype.prettyprint}, ${field.gridDatatype.resolveFlattendSize} >")
  def WB_FieldDatatype(gridDatatype : IR_Datatype) = IR_SpecialDatatype(s"Field< ${gridDatatype.resolveBaseDatatype.prettyprint}, ${gridDatatype.resolveFlattendSize} >")
}
