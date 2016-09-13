package exastencils.datastructures.ir.iv

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.prettyprinting._

/// variables and flags

case class CurrentSlot(var field : Field, var fragmentIdx : IR_Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, true, true, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index, field.level, IR_NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveName = s"currentSlot" + resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, field.level.toString, "")
  override def resolveDatatype = "int"
  override def resolveDefValue = Some(IR_IntegerConstant(0))
}

case class IndexFromField(var layoutIdentifier : String, var level : IR_Expression, var indexId : String, var dim : Int, var fragmentIdx : IR_Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, true, true, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, layoutIdentifier, level, IR_NullExpression)

  override def usesFieldArrays : Boolean = false
  override def usesLevelArrays : Boolean = true

  override def resolveName = s"idx$indexId" + resolvePostfix(fragmentIdx.prettyprint, "", layoutIdentifier, level.prettyprint, "") + s"_${ dimToString(dim) }"
  override def resolveDatatype = IR_IntegerDatatype

  override def getCtor() : Option[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val oldLev = level
    for (l <- Knowledge.minLevel to Knowledge.maxLevel) {
      level = l
      val field = FieldCollection.getFieldByLayoutIdentifier(layoutIdentifier, l, true)
      if (field.isDefined) {
        statements += AssignmentStatement(resolveAccess(resolveName, fragmentIdx, IR_NullExpression, layoutIdentifier, level, IR_NullExpression),
          field.get.fieldLayout.defIdxById(indexId, dim))
      } else {
        // no field found -> try external fields
        val extField = ExternalFieldCollection.getFieldByLayoutIdentifier(layoutIdentifier, l, true)
        if (extField.isDefined) {
          statements += AssignmentStatement(resolveAccess(resolveName, fragmentIdx, IR_NullExpression, layoutIdentifier, level, IR_NullExpression),
            extField.get.fieldLayout.defIdxById(indexId, dim))
        } else {
          // doesn't exist on this level
        }
      }
    }
    level = oldLev
    Some(new LoopOverFragments(statements))
  }
}

/// memory management

abstract class AbstractFieldData extends InternalVariable(true, false, true, true, false) {
  var field : Field
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
      wrappedBody = new ForLoopStatement(
        VariableDeclarationStatement(IR_IntegerDatatype, "slot", Some(0)),
        IR_LowerExpression("slot", field.numSlots),
        IR_PreIncrementExpression("slot"),
        wrappedBody)
    return super.wrapInLoops(wrappedBody)
  }

  override def getCtor() : Option[IR_Statement] = {
    val origSlot = slot
    slot = "slot"
    val ret = Some(wrapInLoops(AssignmentStatement(resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt), resolveDefValue.get)))
    slot = origSlot
    ret
  }

  override def getDtor() : Option[IR_Statement] = {
    val origSlot = slot
    slot = "slot"
    val access = resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt)

    val ret = Some(wrapInLoops(
      new ConditionStatement(access,
        ListBuffer[IR_Statement](
          FreeStatement(access),
          new AssignmentStatement(access, 0)))))
    slot = origSlot
    ret
  }

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = super.resolveAccess(baseAccess, fragment, domain, field, level, neigh)
    if (this.field.numSlots > 1)
      access = new ArrayAccess(access, slot)
    return access
  }
}

case class FieldDataBasePtr(override var field : Field, override var level : IR_Expression, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = LoopOverFragments.defIt) extends AbstractFieldData {
  override def resolveName = (if (1 == field.numSlots) s"fieldData" else "slottedFieldData") +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, level.prettyprint, "") +
    "_base"
}

case class FieldData(override var field : Field, override var level : IR_Expression, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = LoopOverFragments.defIt) extends AbstractFieldData {
  def basePtr = FieldDataBasePtr(field, level, slot, fragmentIdx)

  override def resolveName = (if (1 == field.numSlots) s"fieldData" else "slottedFieldData") +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, level.prettyprint, "")

  override def getDtor() : Option[IR_Statement] = {
    if (Knowledge.data_alignFieldPointers) {
      val origSlot = slot
      slot = "slot"
      val access = resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt)
      val ret = Some(wrapInLoops(new AssignmentStatement(access, 0)))
      slot = origSlot
      ret
    } else {
      super.getDtor()
    }
  }

  override def registerIV(declarations : HashMap[String, VariableDeclarationStatement], ctors : HashMap[String, IR_Statement], dtors : HashMap[String, IR_Statement]) = {
    declarations += (resolveName -> getDeclaration)
    ctors += (resolveName -> getCtor().get)
    dtors += (resolveName -> getDtor().get)

    if (Knowledge.data_alignFieldPointers)
      basePtr.registerIV(declarations, ctors, dtors)
  }
}
