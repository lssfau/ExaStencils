package exastencils.datastructures.ir.iv

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.prettyprinting._

abstract class InternalVariable(var canBePerFragment : Boolean, var canBePerDomain : Boolean, var canBePerField : Boolean, var canBePerLevel : Boolean, var canBePerNeighbor : Boolean) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << resolveName

  def usesFragmentArrays : Boolean = true
  def usesDomainArrays : Boolean = true
  def usesFieldArrays : Boolean = true
  def usesLevelArrays : Boolean = true
  def usesNeighborArrays : Boolean = true

  def resolveName : String
  def resolveDataType : Datatype
  def resolveDefValue : Option[Expression] = None

  def getDeclaration() : VariableDeclarationStatement = {
    var dt : Datatype = resolveDataType

    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      dt = ArrayDatatype(dt, Knowledge.domain_numFragmentsPerBlock)
    if (canBePerDomain && usesDomainArrays && DomainCollection.domains.size > 1)
      dt = ArrayDatatype(dt, DomainCollection.domains.size)
    if (canBePerField && usesFieldArrays && FieldCollection.fields.size > 1)
      dt = ArrayDatatype(dt, FieldCollection.fields.size)
    if (canBePerLevel && usesLevelArrays && Knowledge.numLevels > 1)
      dt = ArrayDatatype(dt, Knowledge.numLevels)
    if (canBePerNeighbor && usesNeighborArrays && Fragment.neighbors.size > 1)
      dt = ArrayDatatype(dt, Fragment.neighbors.size)

    new VariableDeclarationStatement(dt, resolveName)
  }

  def wrapInLoops(body : Statement) : Statement = {
    var wrappedBody = body

    // NOTE: reverse order due to wrapping
    if (canBePerNeighbor && usesNeighborArrays && Fragment.neighbors.size > 1)
      wrappedBody = new LoopOverNeighbors(wrappedBody)
    if (canBePerLevel && usesLevelArrays && Knowledge.numLevels > 1)
      wrappedBody = new LoopOverLevels(wrappedBody)
    if (canBePerField && usesFieldArrays && FieldCollection.fields.size > 1)
      wrappedBody = new LoopOverFields(wrappedBody)
    if (canBePerDomain && usesDomainArrays && DomainCollection.domains.size > 1)
      wrappedBody = new LoopOverDomains(wrappedBody)
    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      wrappedBody = new LoopOverFragments(wrappedBody)

    wrappedBody
  }

  def getCtor() : Option[Statement] = {
    if (resolveDefValue.isDefined)
      Some(wrapInLoops(AssignmentStatement(resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt), resolveDefValue.get)))
    else
      None
  }

  def getDtor() : Option[Statement] = None

  def resolvePostfix(fragment : String, domain : String, field : String, level : String, neigh : String) : String = {
    var postfix : String = ""

    if (canBePerFragment && !usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      postfix += "_" + fragment
    if (canBePerDomain && !usesDomainArrays && DomainCollection.domains.size > 1)
      postfix += "_" + domain
    if (canBePerField && !usesFieldArrays && FieldCollection.fields.size > 1)
      postfix += "_" + field
    if (canBePerLevel && !usesLevelArrays && Knowledge.numLevels > 1)
      postfix += "_" + level
    if (canBePerNeighbor && !usesNeighborArrays && Fragment.neighbors.size > 1)
      postfix += "_" + neigh

    postfix
  }

  def resolveAccess(baseAccess : Expression, fragment : Expression, domain : Expression, field : Expression, level : Expression, neigh : Expression) : Expression = {
    var access = baseAccess

    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      access = new ArrayAccess(access, fragment)
    if (canBePerDomain && usesDomainArrays && DomainCollection.domains.size > 1)
      access = new ArrayAccess(access, domain)
    if (canBePerField && usesFieldArrays && FieldCollection.fields.size > 1)
      access = new ArrayAccess(access, field)
    if (canBePerLevel && usesLevelArrays && Knowledge.numLevels > 1) {
      val simplifiedLevel : Expression =
        if (level.isInstanceOf[IntegerConstant])
          level.asInstanceOf[IntegerConstant].value - Knowledge.minLevel
        else
          level - Knowledge.minLevel
      access = new ArrayAccess(access, simplifiedLevel)
    }
    if (canBePerNeighbor && usesNeighborArrays && Fragment.neighbors.size > 1)
      access = new ArrayAccess(access, neigh)

    access
  }

  def registerIV(declarations : HashMap[String, VariableDeclarationStatement], ctors : HashMap[String, Statement], dtors : HashMap[String, Statement]) = {
    declarations += (resolveName -> getDeclaration)
    if (getCtor().isDefined)
      ctors += (resolveName -> getCtor().get)
    if (getDtor().isDefined)
      dtors += (resolveName -> getDtor().get)
  }
}

abstract class CommVariable extends InternalVariable(Knowledge.comm_sepDataByFragment, false, Knowledge.comm_useFieldArrays, Knowledge.comm_useLevelArrays, Knowledge.comm_useNeighborArrays) {
  override def usesFragmentArrays : Boolean = Knowledge.comm_useFragmentArrays
  override def usesDomainArrays : Boolean = Knowledge.comm_useDomainArrays
  override def usesFieldArrays : Boolean = Knowledge.comm_useFieldArrays
  override def usesLevelArrays : Boolean = Knowledge.comm_useLevelArrays
  override def usesNeighborArrays : Boolean = Knowledge.comm_useNeighborArrays
}

abstract class NeighInfoVariable extends InternalVariable(true, true, false, false, true) {
  override def usesFragmentArrays : Boolean = true
  override def usesDomainArrays : Boolean = true
  override def usesNeighborArrays : Boolean = true
}

abstract class UnduplicatedVariable extends InternalVariable(false, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveName
}

case class RemoteReqOutstanding(var field : Field, var direction : String, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"remoteReqOutstanding_${direction}" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDataType = BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class LocalReqOutstanding(var field : Field, var direction : String, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"localReqOutstanding_${direction}" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDataType = BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class MpiRequest(var field : Field, var direction : String, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"mpiRequest_${direction}" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDataType = "MPI_Request"
}

case class LocalCommReady(var field : Field, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"localCommReady" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDataType = VolatileDatatype(BooleanDatatype)
  override def resolveDefValue = Some(false)
}

case class LocalCommDone(var field : Field, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"localCommDone" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDataType = VolatileDatatype(BooleanDatatype)
  override def resolveDefValue = Some(false)
}

case class NeighborIsValid(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends NeighInfoVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, NullExpression, NullExpression, neighIdx)

  override def resolveName = s"neighbor_isValid" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDataType = BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class NeighborIsRemote(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends NeighInfoVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, NullExpression, NullExpression, neighIdx)

  override def resolveName = s"neighbor_isRemote" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDataType = BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class NeighborFragLocalId(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends NeighInfoVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, NullExpression, NullExpression, neighIdx)

  override def resolveName = s"neighbor_fragCommId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDataType = IntegerDatatype
  override def resolveDefValue = Some(-1)
}

case class NeighborRemoteRank(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends NeighInfoVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, NullExpression, NullExpression, neighIdx)

  override def resolveName = s"neighbor_remoteRank" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDataType = IntegerDatatype
  override def resolveDefValue = Some("MPI_PROC_NULL")
}

case class IsValidForSubdomain(var domain : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"isValidForSubdomain" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDataType = BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class PrimitiveId(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"primitiveId" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDataType = "size_t"
  override def resolveDefValue = Some(-1)
}

case class CommId(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"commId" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDataType = IntegerDatatype
  override def resolveDefValue = Some(-1)
}

case class PrimitivePosition(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"pos" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDataType = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

case class PrimitivePositionBegin(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"posBegin" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDataType = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

case class PrimitivePositionEnd(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"posEnd" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDataType = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

case class IterationOffsetBegin(var domain : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"iterationOffsetBegin" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDataType = "Vec3i"
  override def resolveDefValue = Some("Vec3i(1, 1, 1)")
}

case class IterationOffsetEnd(var domain : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"iterationOffsetEnd" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDataType = "Vec3i"
  override def resolveDefValue = Some("Vec3i(-1, -1, -1)")
}

case class PrimitiveTransformation(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"trafoMatrix" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDataType = "Mat4"
  override def resolveDefValue = Some("Mat4()")
}

case class Timer(var name : Expression) extends UnduplicatedVariable {
  // TODO: strip result of resolveName (no spaces, etc.)
  override def resolveName = s"timer_" + name.prettyprint
  override def resolveDataType = "StopWatch"

  override def getCtor() : Option[Statement] = {
    Some(AssignmentStatement(resolveName ~ ".timerName", "\"" ~ name ~ "\""))
  }
}

case class CurrentSlot(var field : Field, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, true, true, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index, field.level, NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveName = s"currentSlot" + resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, field.level.toString, "")
  override def resolveDataType = "int"
  override def resolveDefValue = Some(IntegerConstant(0))
}

case class IndexFromField(var fieldIdentifier : String, var level : Expression, var indexId : String) extends InternalVariable(false, false, true, true, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, NullExpression, NullExpression, fieldIdentifier, level, NullExpression)

  override def usesFieldArrays : Boolean = false
  override def usesLevelArrays : Boolean = true

  override def resolveName = s"idx$indexId" + resolvePostfix("", "", fieldIdentifier, level.prettyprint, "")
  override def resolveDataType = s"Vec${Knowledge.dimensionality}i"

  override def getCtor() : Option[Statement] = {
    var statements : ListBuffer[Statement] = ListBuffer()
    val oldLev = level
    for (l <- Knowledge.minLevel to Knowledge.maxLevel) {
      level = l
      val field = FieldCollection.getFieldByIdentifier(fieldIdentifier, l, true)
      if (field.isDefined) {
        statements += AssignmentStatement(resolveAccess(resolveName, NullExpression, NullExpression, fieldIdentifier, level, NullExpression),
          s"Vec${Knowledge.dimensionality}i(${
            (0 until Knowledge.dimensionality).map(i => field.get.fieldLayout(i).idxById(indexId).prettyprint).mkString(", ")
          })")
      }
    }
    level = oldLev
    Some(Scope(statements))
  }
}

abstract class AbstractFieldData extends InternalVariable(true, false, true, true, false) {
  var field : Field
  var level : Expression
  var slot : Expression
  var fragmentIdx : Expression

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index, level, NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveDataType = {
    if (field.numSlots > 1)
      new ArrayDatatype(new PointerDatatype(field.dataType.resolveUnderlyingDatatype), field.numSlots)
    else
      new PointerDatatype(field.dataType.resolveUnderlyingDatatype)
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

case class FieldDataBasePtr(var field : Field, var level : Expression, var slot : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends AbstractFieldData {
  override def resolveName = (if (1 == field.numSlots) s"fieldData" else "slottedFieldData") +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, level.prettyprint, "") +
    "_base"
}

case class FieldData(var field : Field, var level : Expression, var slot : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends AbstractFieldData {
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

abstract class AbstractTmpBuffer extends CommVariable {
  var field : Field
  var direction : String
  var size : Expression
  var neighIdx : Expression
  var fragmentIdx : Expression

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)

  override def resolveDataType = new PointerDatatype(field.dataType.resolveUnderlyingDatatype)
  override def resolveDefValue = Some(0)

  override def getDtor() : Option[Statement] = {
    val ptrExpr = resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)
    Some(wrapInLoops(
      new ConditionStatement(ptrExpr,
        ListBuffer[Statement](
          FreeStatement(ptrExpr),
          new AssignmentStatement(ptrExpr, 0)))))
  }
}

case class TmpBufferBasePtr(var field : Field, var direction : String, var size : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends AbstractTmpBuffer {
  override def resolveName = s"buffer_${direction}" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint) + "_base"

}

case class TmpBuffer(var field : Field, var direction : String, var size : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends AbstractTmpBuffer {
  def basePtr = TmpBufferBasePtr(field, direction, size, neighIdx, fragmentIdx)

  override def resolveName = s"buffer_${direction}" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)

  override def getDtor() : Option[Statement] = {
    if (Knowledge.data_alignTmpBufferPointers) {
      var access = resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt)
      Some(wrapInLoops(new AssignmentStatement(access, 0)))
    } else {
      super.getDtor()
    }
  }

  override def registerIV(declarations : HashMap[String, VariableDeclarationStatement], ctors : HashMap[String, Statement], dtors : HashMap[String, Statement]) = {
    declarations += (resolveName -> getDeclaration)
    ctors += (resolveName -> getCtor().get)
    dtors += (resolveName -> getDtor().get)

    if (Knowledge.data_alignTmpBufferPointers)
      basePtr.registerIV(declarations, ctors, dtors)
  }
}
