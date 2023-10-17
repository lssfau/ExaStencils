package exastencils.waLBerla.ir.gpu

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.communication.IR_WaLBerlaCommVariable
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember


abstract class GPU_WaLBerlaGPUFlags extends IR_WaLBerlaInterfaceMember(true, true, false) {

  // TODO: set indexOfRefinedNeighbor correctly
  var indexOfRefinedNeighbor : Option[IR_Expression] = None

  def field : IR_FieldLike

  def fragmentIdx : IR_Expression

  def slot : IR_Expression

  def level : IR_Expression

  override def isPrivate : Boolean = true

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index, level, IR_NullExpression)

  override def resolveDefValue() = Some(false)

  override def resolveDatatype() : IR_Datatype = {
    var dt : IR_Datatype = IR_BooleanDatatype

    if (field.numSlots > 1)
      dt = IR_StdArrayDatatype(dt, field.numSlots)
    dt
  }

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    val access = super.resolveAccess(baseAccess, fragment, domain, field, level, neigh)

    if (this.field.numSlots > 1) IR_ArrayAccess(access, slot) else access
  }

  override def wrapInLoops(body : IR_Statement) : IR_Statement = {
    var wrappedBody = super.wrapInLoops(body)
    if (field.numSlots > 1)
      wrappedBody = IR_ForLoop(
        IR_VariableDeclaration(IR_IntegerDatatype, "slot", 0),
        IR_Lower("slot", field.numSlots),
        IR_PreIncrement("slot"),
        wrappedBody)
    wrappedBody
  }
}

/// GPU_WaLBerlaHostDataUpdated

case class GPU_WaLBerlaHostDataUpdated(var field : IR_FieldLike, var slot : IR_Expression, var level : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends GPU_WaLBerlaGPUFlags {

  override def name : String = s"wbHostDataUpdated_${field.name}"
}

/// GPU_WaLBerlaDeviceDataUpdated

case class GPU_WaLBerlaDeviceDataUpdated(var field : IR_FieldLike, var slot : IR_Expression, var level : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends GPU_WaLBerlaGPUFlags {

  override def name : String = s"wbDeviceDataUpdated_${field.name}"
}

/// GPU_WaLBerlaHostBufferDataUpdated

case class GPU_WaLBerlaHostBufferDataUpdated(
    var field : IR_FieldLike,
    var send : Boolean,
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaCommVariable {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveDefValue() = Some(false)

  override def baseDatatype : IR_Datatype = IR_BooleanDatatype

  override def name : String = s"wbHostBufferDataUpdated_${field.name}_$direction"
}

/// GPU_WaLBerlaDeviceBufferDataUpdated

case class GPU_WaLBerlaDeviceBufferDataUpdated(
    var field : IR_FieldLike,
    var send : Boolean,
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaCommVariable {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveDefValue() = Some(false)

  override def baseDatatype : IR_Datatype = IR_BooleanDatatype

  override def name : String = s"wbDeviceBufferDataUpdated_${field.name}_$direction"
}
