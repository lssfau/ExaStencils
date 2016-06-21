package exastencils.datastructures.ir.iv

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.prettyprinting._

/// variables and flags

case class CurrentSlot(var field : Field, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, true, true, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index, field.level, NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveName = s"currentSlot" + resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, field.level.toString, "")
  override def resolveDataType = "int"
  override def resolveDefValue = Some(IntegerConstant(0))
}

case class IndexFromField(var layoutIdentifier : String, var level : Expression, var indexId : String, var dim : Int, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, true, true, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, layoutIdentifier, level, NullExpression)

  override def usesFieldArrays : Boolean = false
  override def usesLevelArrays : Boolean = true

  override def resolveName = s"idx$indexId" + resolvePostfix(fragmentIdx.prettyprint, "", layoutIdentifier, level.prettyprint, "") + s"_${dimToString(dim)}"
  override def resolveDataType = IntegerDatatype

  override def getCtor() : Option[Statement] = {
    var statements : ListBuffer[Statement] = ListBuffer()
    val oldLev = level
    for (l <- Knowledge.minLevel to Knowledge.maxLevel) {
      level = l
      val field = FieldCollection.getFieldByLayoutIdentifier(layoutIdentifier, l, true)
      if (field.isDefined) {
        statements += AssignmentStatement(resolveAccess(resolveName, fragmentIdx, NullExpression, layoutIdentifier, level, NullExpression),
          field.get.fieldLayout.defIdxById(indexId, dim))
      } else { // no field found -> try external fields
        val extField = ExternalFieldCollection.getFieldByLayoutIdentifier(layoutIdentifier, l, true)
        if (extField.isDefined) {
          statements += AssignmentStatement(resolveAccess(resolveName, fragmentIdx, NullExpression, layoutIdentifier, level, NullExpression),
            extField.get.fieldLayout.defIdxById(indexId, dim))
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
  var level : Expression
  var slot : Expression
  var fragmentIdx : Expression

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index, level, NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveDataType = {
    if (field.numSlots > 1)
      new ArrayDatatype(new PointerDatatype(field.resolveDeclType), field.numSlots)
    else
      new PointerDatatype(field.resolveDeclType)
  }

  override def resolveDefValue = Some(0)

  override def wrapInLoops(body : Statement) : Statement = {
    var wrappedBody = super.wrapInLoops(body)
    if (field.numSlots > 1)
      wrappedBody = new ForLoopStatement(
        VariableDeclarationStatement(IntegerDatatype, "slot", Some(0)),
        LowerExpression("slot", field.numSlots),
        PreIncrementExpression("slot"),
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

  override def getDtor() : Option[Statement] = {
    val origSlot = slot
    slot = "slot"
    var access = resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt)

    val ret = Some(wrapInLoops(
      new ConditionStatement(access,
        ListBuffer[Statement](
          FreeStatement(access),
          new AssignmentStatement(access, 0)))))
    slot = origSlot
    ret
  }

  override def resolveAccess(baseAccess : Expression, fragment : Expression, domain : Expression, field : Expression, level : Expression, neigh : Expression) : Expression = {
    val access = (if (this.field.numSlots > 1) new ArrayAccess(baseAccess, slot) else baseAccess)
    super.resolveAccess(access, fragment, domain, field, level, neigh)
  }
}

case class FieldDataBasePtr(override var field : Field, override var level : Expression, override var slot : Expression, override var fragmentIdx : Expression = LoopOverFragments.defIt) extends AbstractFieldData {
  override def resolveName = (if (1 == field.numSlots) s"fieldData" else "slottedFieldData") +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, level.prettyprint, "") +
    "_base"
}

case class FieldData(override var field : Field, override var level : Expression, override var slot : Expression, override var fragmentIdx : Expression = LoopOverFragments.defIt) extends AbstractFieldData {
  def basePtr = FieldDataBasePtr(field, level, slot, fragmentIdx)

  override def resolveName = (if (1 == field.numSlots) s"fieldData" else "slottedFieldData") +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, level.prettyprint, "")

  override def getDtor() : Option[Statement] = {
    if (Knowledge.data_alignFieldPointers) {
      val origSlot = slot
      slot = "slot"
      var access = resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt)
      val ret = Some(wrapInLoops(new AssignmentStatement(access, 0)))
      slot = origSlot
      ret
    } else {
      super.getDtor()
    }
  }

  override def registerIV(declarations : HashMap[String, VariableDeclarationStatement], ctors : HashMap[String, Statement], dtors : HashMap[String, Statement]) = {
    declarations += (resolveName -> getDeclaration)
    ctors += (resolveName -> getCtor().get)
    dtors += (resolveName -> getDtor().get)

    if (Knowledge.data_alignFieldPointers)
      basePtr.registerIV(declarations, ctors, dtors)
  }
}
