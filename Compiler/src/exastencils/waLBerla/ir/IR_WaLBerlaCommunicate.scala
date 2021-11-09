package exastencils.waLBerla.ir

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_CommScheme
import exastencils.waLBerla.ir.IR_WaLBerlaUtil.getGeneratedName
import exastencils.waLBerla.ir.IR_WaLBerlaUtil.stencilTemplate


case class IR_WaLBerlaCommScheme(
    var wbField : IR_WaLBerlaField,
) extends IR_Access {

  def numSlots = wbField.numSlots

  def ctorInitializerList(blocks : IR_VariableAccess) = if (numSlots > 1)
    Tuple2(baseAccess(), IR_InitializerList((0 until numSlots).map(_ => IR_FunctionCall(IR_ExternalFunctionReference(basetype.prettyprint()), blocks)) : _*))
  else
    Tuple2(baseAccess(), blocks)

  def basetype = WB_CommScheme(stencilTemplate(wbField.numDimsGrid))

  def datatype : IR_Datatype = {
    if (numSlots > 1) IR_ArrayDatatype(basetype, numSlots) else basetype
  }

  def baseAccess() = IR_VariableAccess(resolveName(), datatype)

  def resolveName() = getGeneratedName(s"commScheme_${ wbField.codeName }")

  def resolveAccess(slot : IR_Expression) = {
    if (numSlots > 1) IR_ArrayAccess(baseAccess(), slot) else baseAccess()
  }

  override def prettyprint(out : PpStream) : Unit = out << baseAccess().prettyprint()
}

case class IR_WaLBerlaCommunicate(
    var commScheme: IR_WaLBerlaCommScheme,
    var slot : IR_Expression
) extends IR_Statement {

  def resolveAccess() = {
    IR_MemberFunctionCall(commScheme.resolveAccess(slot), "communicate")
  }

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess().prettyprint() << ";"
}
