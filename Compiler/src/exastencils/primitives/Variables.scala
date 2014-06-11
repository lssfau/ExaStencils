package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.util._

abstract class FragCommMember(var canBePerDomain : Boolean, var canBePerField : Boolean, var canBePerLevel : Boolean, var canBePerNeigh : Boolean) extends Expression {
  override def cpp : String = "curFragment." + resolveName

  def resolveName : String
  def resolveDataType : Datatype
  def resolveDefValue : Option[Expression] = None

  def getDeclaration() : VariableDeclarationStatement = {
    var dt : Datatype = resolveDataType

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

    if (canBePerDomain && Knowledge.comm_sepCommStructsPerDomain && Knowledge.comm_useCommArraysPerDomain)
      wrappedBody = new ForLoopStatement(s"unsigned int d = 0", s"d < ${DomainCollection.domains.size}", s"++d", wrappedBody)
    if (canBePerField && Knowledge.comm_sepCommStructsPerField && Knowledge.comm_useCommArraysPerField)
      wrappedBody = new ForLoopStatement(s"unsigned int f = 0", s"f < ${FieldCollection.fields.size}", s"++f", wrappedBody)
    if (canBePerLevel && Knowledge.comm_sepCommStructsPerLevel && Knowledge.comm_useCommArraysPerLevel)
      wrappedBody = new ForLoopStatement(s"unsigned int l = 0", s"l < ${Knowledge.numLevels}", s"++l", wrappedBody)
    if (canBePerNeigh && Knowledge.comm_sepCommStructsPerNeigh && Knowledge.comm_useCommArraysPerNeigh)
      wrappedBody = new ForLoopStatement(s"unsigned int n = 0", s"n < ${StateManager.findFirst[FragmentClass]().get.neighbors.size}", s"++n", wrappedBody)

    wrappedBody
  }

  def getCtor() : Option[Statement] = {
    if (resolveDefValue.isDefined)
      Some(wrapInLoops(AssignmentStatement(resolveAccess(resolveName, "d", "f", "l", "n"), resolveDefValue.get)))
    else
      None
  }

  def getDtor() : Option[Statement] = None

  def resolvePostfix(domain : String, field : String, level : String, neigh : String) : String = {
    var postfix : String = ""

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

  def resolveAccess(baseAccess : Expression, domain : Expression, field : Expression, level : Expression, neigh : Expression) : Expression = {
    var access = baseAccess

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

case class FragMember_ReqOutstanding(var field : Field, var direction : String, var neighIdx : Expression) extends FragCommMember(false, true, true, true) {
  override def cpp : String = "curFragment." + resolveAccess(resolveName, new NullExpression, field.identifier /*FIXME: id*/ , field.level, neighIdx).cpp

  override def resolveName = s"reqOutstanding_${direction}" + resolvePostfix("", field.identifier, field.level.toString, neighIdx.cpp)
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class FragMember_MpiRequest(var field : Field, var direction : String, var neighIdx : Expression) extends FragCommMember(false, true, true, true) {
  override def cpp : String = "curFragment." + resolveAccess(resolveName, new NullExpression, field.identifier /*FIXME: id*/ , field.level, neighIdx).cpp

  override def resolveName = s"mpiRequest_${direction}" + resolvePostfix("", field.identifier, field.level.toString, neighIdx.cpp)
  override def resolveDataType = "MPI_Request"
}

case class FragMember_TmpBuffer(var field : Field, var direction : String, var size : Expression, var neighIdx : Expression) extends FragCommMember(false, true, true, true) {
  override def cpp : String = "curFragment." + resolveAccess(resolveName, new NullExpression, field.identifier /*FIXME: id*/ , field.level, neighIdx).cpp

  override def resolveName = s"buffer_${direction}" + resolvePostfix("", field.identifier, field.level.toString, neighIdx.cpp)
  override def resolveDataType = new PointerDatatype(field.dataType)
  override def resolveDefValue = Some(0)

  override def getDtor() : Option[Statement] = {
    Some(wrapInLoops(new ConditionStatement(resolveAccess(resolveName, field.domain, field.identifier /*FIXME: use idx*/ , field.level, neighIdx),
      ListBuffer[Statement](
        "delete []" ~~ resolveAccess(resolveName, field.domain, field.identifier /*FIXME: use idx*/ , field.level, neighIdx),
        new AssignmentStatement(resolveAccess(resolveName, field.domain, field.identifier /*FIXME: use idx*/ , field.level, neighIdx), 0)))))
  }
}

case class FragMember_IsValidForSubdomain(var domain : Expression) extends FragCommMember(true, false, false, false) {
  override def cpp : String = "curFragment." + resolveAccess(resolveName, domain, new NullExpression, new NullExpression, new NullExpression).cpp

  override def resolveName = s"isValidForSubdomain" + resolvePostfix(domain.cpp, "", "", "")
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class FragMember_NeighborIsValid(var domain : Expression, var neighIdx : Expression) extends FragCommMember(true, false, false, true) {
  override def cpp : String = "curFragment." + resolveAccess(resolveName, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_isValid" + resolvePostfix(domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class FragMember_NeighborIsRemote(var domain : Expression, var neighIdx : Expression) extends FragCommMember(true, false, false, true) {
  override def cpp : String = "curFragment." + resolveAccess(resolveName, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_isRemote" + resolvePostfix(domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class FragMember_NeighborLocalPtr(var domain : Expression, var neighIdx : Expression) extends FragCommMember(true, false, false, true) {
  override def cpp : String = "curFragment." + resolveAccess(resolveName, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_localPtr" + resolvePostfix(domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new PointerDatatype("Fragment3DCube")
  override def resolveDefValue = Some(0)
}

case class FragMember_NeighborFragCommId(var domain : Expression, var neighIdx : Expression) extends FragCommMember(true, false, false, true) {
  override def cpp : String = "curFragment." + resolveAccess(resolveName, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_fragCommId" + resolvePostfix(domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = "size_t"
  override def resolveDefValue = Some(-1)
}

case class FragMember_NeighborRemoteRank(var domain : Expression, var neighIdx : Expression) extends FragCommMember(true, false, false, true) {
  override def cpp : String = "curFragment." + resolveAccess(resolveName, domain, new NullExpression, new NullExpression, neighIdx).cpp

  override def resolveName = s"neighbor_remoteRank" + resolvePostfix(domain.cpp, "", "", neighIdx.cpp)
  override def resolveDataType = new IntegerDatatype
  override def resolveDefValue = Some("MPI_PROC_NULL")
}
