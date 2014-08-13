package exastencils.datastructures.ir.iv

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._

abstract class InternalVariable(var canBePerFragment : Boolean, var canBePerDomain : Boolean, var canBePerField : Boolean, var canBePerLevel : Boolean, var canBePerNeighbor : Boolean) extends Expression {
  override def cpp(out : CppStream) : Unit = out << resolveName

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

    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragsPerBlock > 1)
      dt = ArrayDatatype(dt, Knowledge.domain_numFragsPerBlock)
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
    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragsPerBlock > 1)
      wrappedBody = new LoopOverFragments(-1, wrappedBody)

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

    if (canBePerFragment && !usesFragmentArrays && Knowledge.domain_numFragsPerBlock > 1)
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

    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragsPerBlock > 1)
      access = new ArrayAccess(access, fragment)
    if (canBePerDomain && usesDomainArrays && DomainCollection.domains.size > 1)
      access = new ArrayAccess(access, domain)
    if (canBePerField && usesFieldArrays && FieldCollection.fields.size > 1)
      access = new ArrayAccess(access, field)
    if (canBePerLevel && usesLevelArrays && Knowledge.numLevels > 1)
      access = new ArrayAccess(access, level)
    if (canBePerNeighbor && usesNeighborArrays && Fragment.neighbors.size > 1)
      access = new ArrayAccess(access, neigh)

    access
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
  override def cpp(out : CppStream) : Unit = out << resolveName
}

case class ReqOutstanding(var field : Field, var direction : String, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, new NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"reqOutstanding_${direction}" + resolvePostfix(fragmentIdx.cpp, "", field.index.toString, field.level.toString, neighIdx.cpp)
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class MpiRequest(var field : Field, var direction : String, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, new NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"mpiRequest_${direction}" + resolvePostfix(fragmentIdx.cpp, "", field.index.toString, field.level.toString, neighIdx.cpp)
  override def resolveDataType = "MPI_Request"
}

case class TmpBuffer(var field : Field, var direction : String, var size : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, new NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"buffer_${direction}" + resolvePostfix(fragmentIdx.cpp, "", field.index.toString, field.level.toString, neighIdx.cpp)
  override def resolveDataType = new PointerDatatype(field.dataType.resolveUnderlyingDatatype)
  override def resolveDefValue = Some(0)

  override def getDtor() : Option[Statement] = {
    Some(wrapInLoops(new ConditionStatement(resolveAccess(resolveName, fragmentIdx, new NullExpression, field.index, field.level, neighIdx),
      ListBuffer[Statement](
        "delete []" ~~ resolveAccess(resolveName, fragmentIdx, new NullExpression, field.index, field.level, neighIdx),
        new AssignmentStatement(resolveAccess(resolveName, fragmentIdx, new NullExpression, field.index, field.level, neighIdx), 0)))))
  }
}

case class NeighborIsValid(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends NeighInfoVariable {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, neighIdx)

  override def resolveName = s"neighbor_isValid" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class NeighborIsRemote(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends NeighInfoVariable {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, neighIdx)

  override def resolveName = s"neighbor_isRemote" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class NeighborFragLocalId(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends NeighInfoVariable {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, neighIdx)

  override def resolveName = s"neighbor_fragCommId" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = "size_t"
  override def resolveDefValue = Some(-1)
}

case class NeighborRemoteRank(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends NeighInfoVariable {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, neighIdx)

  override def resolveName = s"neighbor_remoteRank" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new IntegerDatatype
  override def resolveDefValue = Some("MPI_PROC_NULL")
}

case class IsValidForSubdomain(var domain : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, false) {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, new NullExpression)

  override def resolveName = s"isValidForSubdomain" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", "")
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class FieldData(var field : Field, var slot : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, true, true, false) {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, new NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index, field.level, new NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveName = (if (1 == field.numSlots) s"fieldData" else "slottedFieldData") +
    resolvePostfix(fragmentIdx.cpp, "", if (Knowledge.data_useFieldNamesAsIdx) field.identifier else field.index.toString, field.level.toString, "")

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
      wrappedBody = new ForLoopStatement(s"unsigned int slot = 0", s"slot < ${field.numSlots}", s"++slot", wrappedBody)
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
          if (Knowledge.data_addPrePadding) AssignmentStatement(access, access - field.alignmentPadding) else new NullStatement,
          "delete []" ~~ access,
          new AssignmentStatement(access, 0)))))
    slot = origSlot
    ret
  }

  override def resolveAccess(baseAccess : Expression, fragment : Expression, domain : Expression, field : Expression, level : Expression, neigh : Expression) : Expression = {
    val access = (if (this.field.numSlots > 1) new ArrayAccess(baseAccess, slot) else baseAccess)
    super.resolveAccess(access, fragment, domain, field, level, neigh)
  }
}

case class PrimitiveId(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, new NullExpression, new NullExpression, new NullExpression, new NullExpression)

  override def resolveName = s"primitiveId" + resolvePostfix(fragmentIdx.cpp, "", "", "", "")
  override def resolveDataType = "size_t"
  override def resolveDefValue = Some(-1)
}

case class CommId(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, new NullExpression, new NullExpression, new NullExpression, new NullExpression)

  override def resolveName = s"commId" + resolvePostfix(fragmentIdx.cpp, "", "", "", "")
  override def resolveDataType = new IntegerDatatype
  override def resolveDefValue = Some(-1)
}

case class PrimitivePosition(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, new NullExpression, new NullExpression, new NullExpression, new NullExpression)

  override def resolveName = s"pos" + resolvePostfix(fragmentIdx.cpp, "", "", "", "")
  override def resolveDataType = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

case class PrimitivePositionBegin(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, new NullExpression, new NullExpression, new NullExpression, new NullExpression)

  override def resolveName = s"posBegin" + resolvePostfix(fragmentIdx.cpp, "", "", "", "")
  override def resolveDataType = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

case class PrimitivePositionEnd(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, new NullExpression, new NullExpression, new NullExpression, new NullExpression)

  override def resolveName = s"posEnd" + resolvePostfix(fragmentIdx.cpp, "", "", "", "")
  override def resolveDataType = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

case class IterationOffsetBegin(var domain : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, false) {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, new NullExpression)

  override def resolveName = s"iterationOffsetBegin" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", "")
  override def resolveDataType = "Vec3i"
  override def resolveDefValue = Some("Vec3i(1, 1, 1)")
}

case class IterationOffsetEnd(var domain : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, false) {
  override def cpp(out : CppStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, new NullExpression)

  override def resolveName = s"iterationOffsetEnd" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", "")
  override def resolveDataType = "Vec3i"
  override def resolveDefValue = Some("Vec3i(-1, -1, -1)")
}

case class Timer(var name : Expression) extends UnduplicatedVariable {
  override def resolveName = s"timer_" + name.cpp
  override def resolveDataType = "TimerWrapper"

  override def getCtor() : Option[Statement] = {
    Some(AssignmentStatement(resolveName ~ "._name", "\"" ~ name ~ "\""))
  }
}
