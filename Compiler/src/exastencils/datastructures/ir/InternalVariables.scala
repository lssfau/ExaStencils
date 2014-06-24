package exastencils.datastructures.ir.iv

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.util._

abstract class InternalVariable(var canBePerFragment : Boolean, var canBePerDomain : Boolean, var canBePerField : Boolean, var canBePerLevel : Boolean, var canBePerNeigh : Boolean) extends Expression {
  override def cpp : String = resolveName

  def resolveName : String
  def resolveDataType : Datatype
  def resolveDefValue : Option[Expression] = None

  def getDeclaration() : VariableDeclarationStatement = {
    var dt : Datatype = resolveDataType

    if (canBePerFragment && Knowledge.comm_useCommArraysPerFragment && Knowledge.domain_numFragsPerBlock > 1)
      dt = ArrayDatatype(dt, Knowledge.domain_numFragsPerBlock)
    if (canBePerDomain && Knowledge.comm_sepCommStructsPerDomain && Knowledge.comm_useCommArraysPerDomain && DomainCollection.domains.size > 1)
      dt = ArrayDatatype(dt, DomainCollection.domains.size)
    if (canBePerField && Knowledge.comm_sepCommStructsPerField && Knowledge.comm_useCommArraysPerField && FieldCollection.fields.size > 1)
      dt = ArrayDatatype(dt, FieldCollection.fields.size)
    if (canBePerLevel && Knowledge.comm_sepCommStructsPerLevel && Knowledge.comm_useCommArraysPerLevel && Knowledge.numLevels > 1)
      dt = ArrayDatatype(dt, Knowledge.numLevels)
    if (canBePerNeigh && Knowledge.comm_sepCommStructsPerNeigh && Knowledge.comm_useCommArraysPerNeigh && Fragment.neighbors.size > 1)
      dt = ArrayDatatype(dt, Fragment.neighbors.size)

    new VariableDeclarationStatement(dt, resolveName)
  }

  def wrapInLoops(body : Statement) : Statement = {
    var wrappedBody = body

    // NOTE: reverse order due to wrapping
    if (canBePerNeigh && Knowledge.comm_sepCommStructsPerNeigh && Knowledge.comm_useCommArraysPerNeigh && Fragment.neighbors.size > 1)
      wrappedBody = new LoopOverNeighbors(wrappedBody)
    if (canBePerLevel && Knowledge.comm_sepCommStructsPerLevel && Knowledge.comm_useCommArraysPerLevel && Knowledge.numLevels > 1)
      wrappedBody = new LoopOverLevels(wrappedBody)
    if (canBePerField && Knowledge.comm_sepCommStructsPerField && Knowledge.comm_useCommArraysPerField && FieldCollection.fields.size > 1)
      wrappedBody = new LoopOverFields(wrappedBody)
    if (canBePerDomain && Knowledge.comm_sepCommStructsPerDomain && Knowledge.comm_useCommArraysPerDomain && DomainCollection.domains.size > 1)
      wrappedBody = new LoopOverDomains(wrappedBody)
    if (canBePerFragment && Knowledge.comm_useCommArraysPerFragment && Knowledge.domain_numFragsPerBlock > 1)
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

  def resolvePostfix(domain : String, fragment : String, field : String, level : String, neigh : String) : String = {
    var postfix : String = ""

    if (canBePerFragment && !Knowledge.comm_useCommArraysPerDomain && Knowledge.domain_numFragsPerBlock > 1)
      postfix += "_" + fragment
    if (canBePerDomain && Knowledge.comm_sepCommStructsPerDomain && !Knowledge.comm_useCommArraysPerDomain && DomainCollection.domains.size > 1)
      postfix += "_" + domain
    if (canBePerField && Knowledge.comm_sepCommStructsPerField && !Knowledge.comm_useCommArraysPerField && FieldCollection.fields.size > 1)
      postfix += "_" + field
    if (canBePerLevel && Knowledge.comm_sepCommStructsPerLevel && !Knowledge.comm_useCommArraysPerLevel && Knowledge.numLevels > 1)
      postfix += "_" + level
    if (canBePerNeigh && Knowledge.comm_sepCommStructsPerNeigh && !Knowledge.comm_useCommArraysPerNeigh && Fragment.neighbors.size > 1)
      postfix += "_" + neigh

    postfix
  }

  def resolveAccess(baseAccess : Expression, fragment : Expression, domain : Expression, field : Expression, level : Expression, neigh : Expression) : Expression = {
    var access = baseAccess

    if (canBePerFragment && Knowledge.comm_useCommArraysPerFragment && Knowledge.domain_numFragsPerBlock > 1)
      access = new ArrayAccess(access, fragment)
    if (canBePerDomain && Knowledge.comm_sepCommStructsPerDomain && Knowledge.comm_useCommArraysPerDomain && DomainCollection.domains.size > 1)
      access = new ArrayAccess(access, domain)
    if (canBePerField && Knowledge.comm_sepCommStructsPerField && Knowledge.comm_useCommArraysPerField && FieldCollection.fields.size > 1)
      access = new ArrayAccess(access, field)
    if (canBePerLevel && Knowledge.comm_sepCommStructsPerLevel && Knowledge.comm_useCommArraysPerLevel && Knowledge.numLevels > 1)
      access = new ArrayAccess(access, level)
    if (canBePerNeigh && Knowledge.comm_sepCommStructsPerNeigh && Knowledge.comm_useCommArraysPerNeigh && Fragment.neighbors.size > 1)
      access = new ArrayAccess(access, neigh)

    access
  }
}

case class ReqOutstanding(var field : Field, var direction : String, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, true, true, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, new NullExpression, field.index, field.level, neighIdx).cpp

  override def resolveName = s"reqOutstanding_${direction}" + resolvePostfix(fragmentIdx.cpp, "", field.index.toString, field.level.toString, neighIdx.cpp)
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class MpiRequest(var field : Field, var direction : String, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, true, true, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, new NullExpression, field.index, field.level, neighIdx).cpp

  override def resolveName = s"mpiRequest_${direction}" + resolvePostfix(fragmentIdx.cpp, "", field.index.toString, field.level.toString, neighIdx.cpp)
  override def resolveDataType = "MPI_Request"
}

case class TmpBuffer(var field : Field, var direction : String, var size : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, true, true, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, new NullExpression, field.index, field.level, neighIdx).cpp

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

case class IsValidForSubdomain(var domain : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, false) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, new NullExpression).cpp

  override def resolveName = s"isValidForSubdomain" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", "")
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class NeighborIsValid(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_isValid" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class NeighborIsRemote(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_isRemote" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class NeighborFragLocalId(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_fragCommId" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = "size_t"
  override def resolveDefValue = Some(-1)
}

case class NeighborRemoteRank(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_remoteRank" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new IntegerDatatype
  override def resolveDefValue = Some("MPI_PROC_NULL")
}

case class FieldData(var field : Field, var slot : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, true, false) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, new NullExpression, new NullExpression, field.level, new NullExpression).cpp

  override def resolveName = field.codeName + resolvePostfix(fragmentIdx.cpp, "", "", field.level.toString, "")

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
    val ret = Some(wrapInLoops(new ConditionStatement(resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt),
      ListBuffer[Statement](
        "delete []" ~~ resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt),
        new AssignmentStatement(resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt), 0)))))
    slot = origSlot
    ret
  }

  override def resolveAccess(baseAccess : Expression, fragment : Expression, domain : Expression, field : Expression, level : Expression, neigh : Expression) : Expression = {
    val access = (if (this.field.numSlots > 1) new ArrayAccess(baseAccess, slot) else baseAccess)
    super.resolveAccess(access, fragment, domain, field, level, neigh)
  }
}

case class PrimitiveId(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, new NullExpression, new NullExpression, new NullExpression, new NullExpression).cpp

  override def resolveName = s"primitiveId" + resolvePostfix(fragmentIdx.cpp, "", "", "", "")
  override def resolveDataType = "size_t"
  override def resolveDefValue = Some(-1)
}

case class CommId(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, new NullExpression, new NullExpression, new NullExpression, new NullExpression).cpp

  override def resolveName = s"commId" + resolvePostfix(fragmentIdx.cpp, "", "", "", "")
  override def resolveDataType = new IntegerDatatype
  override def resolveDefValue = Some(-1)
}

case class PrimitivePosition(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, new NullExpression, new NullExpression, new NullExpression, new NullExpression).cpp

  override def resolveName = s"pos" + resolvePostfix(fragmentIdx.cpp, "", "", "", "")
  override def resolveDataType = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

case class PrimitivePositionBegin(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, new NullExpression, new NullExpression, new NullExpression, new NullExpression).cpp

  override def resolveName = s"posBegin" + resolvePostfix(fragmentIdx.cpp, "", "", "", "")
  override def resolveDataType = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

case class PrimitivePositionEnd(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, new NullExpression, new NullExpression, new NullExpression, new NullExpression).cpp

  override def resolveName = s"posEnd" + resolvePostfix(fragmentIdx.cpp, "", "", "", "")
  override def resolveDataType = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

case class IterationOffsetBegin(var domain : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, false) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, new NullExpression).cpp

  override def resolveName = s"iterationOffsetBegin" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", "")
  override def resolveDataType = "Vec3i"
  override def resolveDefValue = Some("Vec3i(1, 1, 1)")
}

case class IterationOffsetEnd(var domain : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, false) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, new NullExpression).cpp

  override def resolveName = s"iterationOffsetEnd" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", "")
  override def resolveDataType = "Vec3i"
  override def resolveDefValue = Some("Vec3i(-1, -1, -1)")
}
