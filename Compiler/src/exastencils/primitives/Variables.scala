package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.util._

abstract class FragCommMember(var canBePerFragment : Boolean, var canBePerDomain : Boolean, var canBePerField : Boolean, var canBePerLevel : Boolean, var canBePerNeigh : Boolean) extends Expression {
  override def cpp : String = "curFragment." + resolveName

  def resolveName : String
  def resolveDataType : Datatype
  def resolveDefValue : Option[Expression] = None

  def getDeclaration() : VariableDeclarationStatement = {
    var dt : Datatype = resolveDataType

    if (canBePerFragment && Knowledge.comm_useCommArraysPerFragment)
      dt = ArrayDatatype(dt, Knowledge.domain_numFragsPerBlock)
    if (canBePerDomain && Knowledge.comm_sepCommStructsPerDomain && Knowledge.comm_useCommArraysPerDomain)
      dt = ArrayDatatype(dt, DomainCollection.domains.size)
    if (canBePerField && Knowledge.comm_sepCommStructsPerField && Knowledge.comm_useCommArraysPerField)
      dt = ArrayDatatype(dt, FieldCollection.fields.size)
    if (canBePerLevel && Knowledge.comm_sepCommStructsPerLevel && Knowledge.comm_useCommArraysPerLevel)
      dt = ArrayDatatype(dt, Knowledge.numLevels)
    if (canBePerNeigh && Knowledge.comm_sepCommStructsPerNeigh && Knowledge.comm_useCommArraysPerNeigh)
      dt = ArrayDatatype(dt, StateManager.findFirst[FragmentClass]().get.neighbors.size)

    new VariableDeclarationStatement(dt, resolveName)
  }

  def wrapInLoops(body : Statement) : Statement = {
    var wrappedBody = body

    // NOTE: reverse order due to wrapping
    if (canBePerNeigh && Knowledge.comm_sepCommStructsPerNeigh && Knowledge.comm_useCommArraysPerNeigh)
      wrappedBody = new ForLoopStatement(s"unsigned int neighIdx = 0", s"neighIdx < ${StateManager.findFirst[FragmentClass]().get.neighbors.size}", s"++neighIdx", wrappedBody)
    if (canBePerLevel && Knowledge.comm_sepCommStructsPerLevel && Knowledge.comm_useCommArraysPerLevel)
      wrappedBody = new ForLoopStatement(s"unsigned int level = 0", s"level < ${Knowledge.numLevels}", s"++level", wrappedBody)
    if (canBePerField && Knowledge.comm_sepCommStructsPerField && Knowledge.comm_useCommArraysPerField)
      wrappedBody = new ForLoopStatement(s"unsigned int fieldIdx = 0", s"fieldIdx < ${FieldCollection.fields.size}", s"++fieldIdx", wrappedBody)
    if (canBePerDomain && Knowledge.comm_sepCommStructsPerDomain && Knowledge.comm_useCommArraysPerDomain)
      wrappedBody = new ForLoopStatement(s"unsigned int domainIdx = 0", s"domainIdx < ${DomainCollection.domains.size}", s"++domainIdx", wrappedBody)
    if (canBePerFragment && Knowledge.comm_useCommArraysPerFragment)
      wrappedBody = new ForLoopStatement(s"unsigned int fragmentIdx = 0", s"fragmentIdx < ${Knowledge.domain_numFragsPerBlock}", s"++fragmentIdx", wrappedBody)

    wrappedBody
  }

  def getCtor() : Option[Statement] = {
    if (resolveDefValue.isDefined)
      Some(wrapInLoops(AssignmentStatement(resolveAccess(resolveName, "fragmentIdx", "domainIdx", "fieldIdx", "level", "neighIdx"), resolveDefValue.get)))
    else
      None
  }

  def getDtor() : Option[Statement] = None

  def resolvePostfix(domain : String, fragment : String, field : String, level : String, neigh : String) : String = {
    var postfix : String = ""

    if (canBePerFragment && !Knowledge.comm_useCommArraysPerDomain)
      postfix += "_" + fragment
    if (canBePerDomain && Knowledge.comm_sepCommStructsPerDomain && !Knowledge.comm_useCommArraysPerDomain)
      postfix += "_" + domain
    if (canBePerField && Knowledge.comm_sepCommStructsPerField && !Knowledge.comm_useCommArraysPerField)
      postfix += "_" + field
    if (canBePerLevel && Knowledge.comm_sepCommStructsPerLevel && !Knowledge.comm_useCommArraysPerLevel)
      postfix += "_" + level
    if (canBePerNeigh && Knowledge.comm_sepCommStructsPerNeigh && !Knowledge.comm_useCommArraysPerNeigh)
      postfix += "_" + neigh

    postfix
  }

  def resolveAccess(baseAccess : Expression, fragment : Expression, domain : Expression, field : Expression, level : Expression, neigh : Expression) : Expression = {
    var access = baseAccess

    if (canBePerFragment && Knowledge.comm_useCommArraysPerFragment)
      access = new ArrayAccess(access, fragment)
    if (canBePerDomain && Knowledge.comm_sepCommStructsPerDomain && Knowledge.comm_useCommArraysPerDomain)
      access = new ArrayAccess(access, domain)
    if (canBePerField && Knowledge.comm_sepCommStructsPerField && Knowledge.comm_useCommArraysPerField)
      access = new ArrayAccess(access, field)
    if (canBePerLevel && Knowledge.comm_sepCommStructsPerLevel && Knowledge.comm_useCommArraysPerLevel)
      access = new ArrayAccess(access, level)
    if (canBePerNeigh && Knowledge.comm_sepCommStructsPerNeigh && Knowledge.comm_useCommArraysPerNeigh)
      access = new ArrayAccess(access, neigh)

    access
  }
}

case class FragMember_ReqOutstanding(var field : Field, var direction : String, var neighIdx : Expression, var fragmentIdx : Expression = "fragmentIdx") extends FragCommMember(true, false, true, true, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, new NullExpression, field.identifier /*FIXME: id*/ , field.level, neighIdx).cpp

  override def resolveName = s"reqOutstanding_${direction}" + resolvePostfix(fragmentIdx.cpp, "", field.identifier, field.level.toString, neighIdx.cpp)
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class FragMember_MpiRequest(var field : Field, var direction : String, var neighIdx : Expression, var fragmentIdx : Expression = "fragmentIdx") extends FragCommMember(true, false, true, true, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, new NullExpression, field.identifier /*FIXME: id*/ , field.level, neighIdx).cpp

  override def resolveName = s"mpiRequest_${direction}" + resolvePostfix(fragmentIdx.cpp, "", field.identifier, field.level.toString, neighIdx.cpp)
  override def resolveDataType = "MPI_Request"
}

case class FragMember_TmpBuffer(var field : Field, var direction : String, var size : Expression, var neighIdx : Expression, var fragmentIdx : Expression = "fragmentIdx") extends FragCommMember(true, false, true, true, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, new NullExpression, field.identifier /*FIXME: id*/ , field.level, neighIdx).cpp

  override def resolveName = s"buffer_${direction}" + resolvePostfix(fragmentIdx.cpp, "", field.identifier, field.level.toString, neighIdx.cpp)
  override def resolveDataType = new PointerDatatype(field.dataType.resolveUnderlyingDatatype)
  override def resolveDefValue = Some(0)

  override def getDtor() : Option[Statement] = {
    Some(wrapInLoops(new ConditionStatement(resolveAccess(resolveName, new NullExpression, field.domain.index, field.identifier /*FIXME: use idx*/ , field.level, neighIdx),
      ListBuffer[Statement](
        "delete []" ~~ resolveAccess(resolveName, new NullExpression, field.domain.index, field.identifier /*FIXME: use idx*/ , field.level, neighIdx),
        new AssignmentStatement(resolveAccess(resolveName, new NullExpression, field.domain.index, field.identifier /*FIXME: use idx*/ , field.level, neighIdx), 0)))))
  }
}

case class FragMember_IsValidForSubdomain(var domain : Expression, var fragmentIdx : Expression = "fragmentIdx") extends FragCommMember(true, true, false, false, false) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, new NullExpression).cpp

  override def resolveName = s"isValidForSubdomain" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", "")
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class FragMember_NeighborIsValid(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = "fragmentIdx") extends FragCommMember(true, true, false, false, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_isValid" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class FragMember_NeighborIsRemote(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = "fragmentIdx") extends FragCommMember(true, true, false, false, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_isRemote" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class FragMember_NeighborLocalPtr(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = "fragmentIdx") extends FragCommMember(true, true, false, false, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_localPtr" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new PointerDatatype("Fragment3DCube")
  override def resolveDefValue = Some(0)
}

case class FragMember_NeighborFragCommId(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = "fragmentIdx") extends FragCommMember(true, true, false, false, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_fragCommId" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = "size_t"
  override def resolveDefValue = Some(-1)
}

case class FragMember_NeighborRemoteRank(var domain : Expression, var neighIdx : Expression, var fragmentIdx : Expression = "fragmentIdx") extends FragCommMember(true, true, false, false, true) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_remoteRank" + resolvePostfix(fragmentIdx.cpp, domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new IntegerDatatype
  override def resolveDefValue = Some("MPI_PROC_NULL")
}

case class FragMember_Field(var field : Field, var fragmentIdx : Expression = "fragmentIdx") extends FragCommMember(true, false, false, true, false) {
  override def cpp : String = resolveAccess(resolveName, fragmentIdx, new NullExpression, new NullExpression, field.level, new NullExpression).cpp

  override def resolveName = /*s"fieldData"*/ field.codeName + resolvePostfix(fragmentIdx.cpp, "", "", field.level.toString, "")
  override def resolveDataType = new PointerDatatype(field.dataType.resolveUnderlyingDatatype)
  override def resolveDefValue = Some(0)

  override def getDeclaration() : VariableDeclarationStatement = {
    var dec = super.getDeclaration
    dec.dataType = new ArrayDatatype(dec.dataType, field.numSlots)
    dec
  }

  override def getCtor() : Option[Statement] = {
    Some(wrapInLoops(StatementBlock(
      (0 until field.numSlots).to[ListBuffer].map(slot =>
        AssignmentStatement(resolveAccess(resolveName, "fragmentIdx", "domainIdx", "fieldIdx", "level", "neighIdx") ~ s"[$slot]", resolveDefValue.get) : Statement))))
  }
}
