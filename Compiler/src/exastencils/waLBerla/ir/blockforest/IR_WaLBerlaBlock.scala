package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.waLBerla.ir.field._
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes._

class IR_WaLBerlaBlock(name : String, dt : IR_Datatype) extends IR_VariableAccess(name, dt) {

  // get field data from block
  def getDataFromBlock(wbField : IR_WaLBerlaField, slot : Int) = {
    val fieldDt = WB_FieldDatatype(wbField)
    new IR_MemberFunctionCallArrow(this, s"getData< ${fieldDt.typeName} >", ListBuffer(IR_WaLBerlaBlockDataID(wbField, slot)), fieldDt)
  }

  def getFields(accesses : IR_WaLBerlaFieldAccess*) : ListBuffer[IR_VariableDeclaration] = {
    accesses.to[mutable.ListBuffer].map(fAcc => {
      val wbField = fAcc.field
      val defValue = if (wbField.numSlots > 1) {
        IR_InitializerList((0 until wbField.numSlots).map(slot => getDataFromBlock(wbField, slot)) : _*)
      } else {
        getDataFromBlock(wbField, 0)
      }
      IR_IV_WaLBerlaFieldData(fAcc).getData(Some(defValue))
    })
  }

  def aabbDatatype = IR_SpecialDatatype("math::AABB")
  def getBlockAABB() = IR_MemberFunctionCallArrow(this, "getAABB", aabbDatatype)
}
