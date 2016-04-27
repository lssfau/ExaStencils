package exastencils.datastructures.ir.iv

import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.prettyprinting._

/// general fragment parameters

case class PrimitiveId(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"primitiveId" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDataType = "size_t"
  override def resolveDefValue = Some(-1)
}

case class IsValidForSubdomain(var domain : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, true, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"isValidForSubdomain" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDataType = BooleanDatatype
  override def resolveDefValue = Some(false)
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

/// geometric mapping

case class CommId(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"commId" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDataType = IntegerDatatype
  override def resolveDefValue = Some(-1)
}

case class PrimitiveIndex(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"primitiveIndex" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDataType = "Vec3i"
  override def resolveDefValue = Some("Vec3i(0, 0, 0)")
}

case class PrimitivePosition(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) with Access {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"pos" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDataType = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

case class PrimitivePositionBegin(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) with Access {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"posBegin" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDataType = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

case class PrimitivePositionEnd(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) with Access {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"posEnd" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDataType = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

case class PrimitiveTransformation(var fragmentIdx : Expression = LoopOverFragments.defIt) extends InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, NullExpression, NullExpression, NullExpression)

  override def resolveName = s"trafoMatrix" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDataType = "Mat4"
  override def resolveDefValue = Some("Mat4()")
}

/// neighborhood information

abstract class NeighInfoVariable extends InternalVariable(true, true, false, false, true) {
  override def usesFragmentArrays : Boolean = true
  override def usesDomainArrays : Boolean = true
  override def usesNeighborArrays : Boolean = true
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

