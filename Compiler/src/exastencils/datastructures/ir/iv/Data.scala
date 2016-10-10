package exastencils.datastructures.ir.iv

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.deprecated.ir.IR_DimToString
import exastencils.field.ir._
import exastencils.interfacing.ir.IR_ExternalFieldCollection
import exastencils.prettyprinting._

/// variables and flags

case class IndexFromField(var layoutIdentifier : String, var level : IR_Expression, var indexId : String, var dim : Int, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, true, true, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, layoutIdentifier, level, IR_NullExpression)

  override def usesFieldArrays : Boolean = false
  override def usesLevelArrays : Boolean = true

  override def resolveName = s"idx$indexId" + resolvePostfix(fragmentIdx.prettyprint, "", layoutIdentifier, level.prettyprint, "") + s"_${ IR_DimToString(dim) }"
  override def resolveDatatype = IR_IntegerDatatype

  override def getCtor() : Option[IR_Statement] = {
    // TODO: refactor -> looking up a field by its layout is not suitable
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val oldLev = level
    for (l <- Knowledge.minLevel to Knowledge.maxLevel) {
      level = l
      val field = IR_FieldCollection.getByLayoutIdentifier(layoutIdentifier, l, true)
      if (field.isDefined) {
        statements += IR_Assignment(resolveAccess(resolveName, fragmentIdx, IR_NullExpression, layoutIdentifier, level, IR_NullExpression),
          field.get.fieldLayout.defIdxById(indexId, dim))
      } else {
        // no field found -> try external fields
        val extField = IR_ExternalFieldCollection.getByLayoutIdentifier(layoutIdentifier, l, true)
        if (extField.isDefined) {
          statements += IR_Assignment(resolveAccess(resolveName, fragmentIdx, IR_NullExpression, layoutIdentifier, level, IR_NullExpression),
            extField.get.fieldLayout.defIdxById(indexId, dim))
        } else {
          // doesn't exist on this level
        }
      }
    }
    level = oldLev
    Some(new IR_LoopOverFragments(statements))
  }
}

/// memory management

abstract class AbstractFieldData extends IR_InternalVariable(true, false, true, true, false) {
  var field : IR_Field
  var level : IR_Expression
  var slot : IR_Expression
  var fragmentIdx : IR_Expression

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index, level, IR_NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveDatatype = {
    if (field.numSlots > 1)
      new IR_ArrayDatatype(new IR_PointerDatatype(field.resolveDeclType), field.numSlots)
    else
      new IR_PointerDatatype(field.resolveDeclType)
  }

  override def resolveDefValue = Some(0)

  override def wrapInLoops(body : IR_Statement) : IR_Statement = {
    var wrappedBody = body
    if (field.numSlots > 1)
      wrappedBody = IR_ForLoop(
        IR_VariableDeclaration(IR_IntegerDatatype, "slot", 0),
        IR_LowerExpression("slot", field.numSlots),
        IR_PreIncrementExpression("slot"),
        wrappedBody)
    return super.wrapInLoops(wrappedBody)
  }

  override def getCtor() : Option[IR_Statement] = {
    val origSlot = slot
    slot = "slot"
    val ret = Some(wrapInLoops(IR_Assignment(resolveAccess(resolveName, IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt), resolveDefValue.get)))
    slot = origSlot
    ret
  }

  override def getDtor() : Option[IR_Statement] = {
    val origSlot = slot
    slot = "slot"
    val access = resolveAccess(resolveName, IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)

    val ret = Some(wrapInLoops(
      IR_IfCondition(access,
        ListBuffer[IR_Statement](
          IR_ArrayFree(access),
          new IR_Assignment(access, 0)))))
    slot = origSlot
    ret
  }

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = super.resolveAccess(baseAccess, fragment, domain, field, level, neigh)
    if (this.field.numSlots > 1)
      access = new IR_ArrayAccess(access, slot)
    return access
  }
}

case class FieldDataBasePtr(override var field : IR_Field, override var level : IR_Expression, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends AbstractFieldData {
  override def resolveName = (if (1 == field.numSlots) s"fieldData" else "slottedFieldData") +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, level.prettyprint, "") +
    "_base"
}

case class FieldData(override var field : IR_Field, override var level : IR_Expression, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends AbstractFieldData {
  def basePtr = FieldDataBasePtr(field, level, slot, fragmentIdx)

  override def resolveName = (if (1 == field.numSlots) s"fieldData" else "slottedFieldData") +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, level.prettyprint, "")

  override def getDtor() : Option[IR_Statement] = {
    if (Knowledge.data_alignFieldPointers) {
      val origSlot = slot
      slot = "slot"
      val access = resolveAccess(resolveName, IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)
      val ret = Some(wrapInLoops(new IR_Assignment(access, 0)))
      slot = origSlot
      ret
    } else {
      super.getDtor()
    }
  }

  override def registerIV(declarations : HashMap[String, IR_VariableDeclaration], ctors : HashMap[String, IR_Statement], dtors : HashMap[String, IR_Statement]) = {
    declarations += (resolveName -> getDeclaration)
    ctors += (resolveName -> getCtor().get)
    dtors += (resolveName -> getDtor().get)

    if (Knowledge.data_alignFieldPointers)
      basePtr.registerIV(declarations, ctors, dtors)
  }
}
