package exastencils.datastructures.ir.iv

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.cuda._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.prettyprinting._

/// general variables and flags

abstract class FieldFlag extends InternalVariable(true, false, true, true, false) {
  var field : Field
  var slot : IR_Expression
  var fragmentIdx : IR_Expression

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    val access = (if (this.field.numSlots > 1) new ArrayAccess(baseAccess, slot) else baseAccess)
    super.resolveAccess(access, fragment, domain, field, level, neigh)
  }

  override def resolveDatatype = {
    if (field.numSlots > 1)
      IR_ArrayDatatype(IR_BooleanDatatype, field.numSlots)
    else
      IR_BooleanDatatype
  }

  override def wrapInLoops(body : Statement) : Statement = {
    var wrappedBody = super.wrapInLoops(body)
    if (field.numSlots > 1)
      wrappedBody = new ForLoopStatement(
        VariableDeclarationStatement(IR_IntegerDatatype, "slot", Some(0)),
        IR_LowerExpression("slot", field.numSlots),
        IR_PreIncrementExpression("slot"),
        wrappedBody)
    wrappedBody
  }

  override def getCtor() : Option[Statement] = {
    val origSlot = slot
    slot = "slot"
    val ret = Some(wrapInLoops(AssignmentStatement(resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt), resolveDefValue.get)))
    slot = origSlot
    ret
  }
}

case class HostDataUpdated(override var field : Field, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = LoopOverFragments.defIt) extends FieldFlag {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index, field.level, NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveName = s"hostDataUpdated" + resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, field.level.toString, "")
  override def resolveDefValue = Some(BooleanConstant(true))
}

case class DeviceDataUpdated(override var field : Field, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = LoopOverFragments.defIt) extends FieldFlag {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index, field.level, NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveName = s"deviceDataUpdated" + resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, field.level.toString, "")
  override def resolveDefValue = Some(BooleanConstant(false))
}

/// memory management

case class FieldDeviceData(override var field : Field, override var level : IR_Expression, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = LoopOverFragments.defIt) extends AbstractFieldData {
  override def resolveName = (if (1 == field.numSlots) s"fieldDeviceData" else "slottedFieldDeviceData") +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, level.prettyprint, "")

  override def getDtor() : Option[Statement] = {
    val origSlot = slot
    slot = "slot"
    var access = resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt)

    val ret = Some(wrapInLoops(
      new ConditionStatement(access,
        ListBuffer[Statement](
          CUDA_FreeStatement(access),
          new AssignmentStatement(access, 0)))))
    slot = origSlot
    ret
  }
}

case class ReductionDeviceData(var size : IR_Expression, var fragmentIdx : IR_Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def resolveDatatype = IR_PointerDatatype(IR_RealDatatype)
  // TODO: extend for other types
  override def resolveName : String = "reductionDeviceData" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")

  override def getDtor() : Option[Statement] = {
    var access = resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt)
    Some(new ConditionStatement(access,
      ListBuffer[Statement](
        CUDA_FreeStatement(access),
        new AssignmentStatement(access, 0))))
  }
}
