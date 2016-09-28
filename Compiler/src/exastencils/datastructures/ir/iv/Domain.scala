package exastencils.datastructures.ir.iv

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.prettyprinting._

/// general fragment parameters

case class PrimitiveId(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"primitiveId" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = "size_t"
  override def resolveDefValue = Some(-1)
}

case class IsValidForSubdomain(var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, true, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"isValidForSubdomain" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype = IR_BooleanDatatype
  override def resolveDefValue = Some(false)
}

/// geometric mapping

case class CommId(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"commId" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = IR_IntegerDatatype
  override def resolveDefValue = Some(-1)
}

// TODO: convert PrimitiveIndex to Int datatype
object PrimitiveIndex {
  def apply(dim : Int) = IR_ArrayAccess(new PrimitiveIndex(), dim)
  def apply(dim : Int, fragmentIdx : IR_Expression) = IR_ArrayAccess(new PrimitiveIndex(fragmentIdx), dim)
}

case class PrimitiveIndex(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"primitiveIndex" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = "Vec3i"
  override def resolveDefValue = Some("Vec3i(0, 0, 0)")
}

// TODO: convert PrimitivePosition to Real datatype
object PrimitivePosition {
  def apply(dim : Int) = IR_ArrayAccess(new PrimitivePosition(), dim)
  def apply(dim : Int, fragmentIdx : IR_Expression) = IR_ArrayAccess(new PrimitivePosition(fragmentIdx), dim)
}

case class PrimitivePosition(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) with IR_Access {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"pos" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

// TODO: convert PrimitivePositionBegin to Real datatype
object PrimitivePositionBegin {
  def apply(dim : Int) = IR_ArrayAccess(new PrimitivePositionBegin(), dim)
  def apply(dim : Int, fragmentIdx : IR_Expression) = IR_ArrayAccess(new PrimitivePositionBegin(fragmentIdx), dim)
}

case class PrimitivePositionBegin(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) with IR_Access {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"posBegin" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

// TODO: convert PrimitivePositionEnd to Real datatype
object PrimitivePositionEnd {
  def apply(dim : Int) = IR_ArrayAccess(new PrimitivePositionEnd(), dim)
  def apply(dim : Int, fragmentIdx : IR_Expression) = IR_ArrayAccess(new PrimitivePositionEnd(fragmentIdx), dim)
}

case class PrimitivePositionEnd(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) with IR_Access {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"posEnd" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

case class PrimitiveTransformation(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"trafoMatrix" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = "Mat4"
  override def resolveDefValue = Some("Mat4()")
}

/// neighborhood information

abstract class NeighInfoVariable extends IR_InternalVariable(true, true, false, false, true) {
  override def usesFragmentArrays : Boolean = true
  override def usesDomainArrays : Boolean = true
  override def usesNeighborArrays : Boolean = true
}

case class NeighborIsValid(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends NeighInfoVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName = s"neighbor_isValid" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype = IR_BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class NeighborIsRemote(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends NeighInfoVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName = s"neighbor_isRemote" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype = IR_BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class NeighborFragLocalId(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends NeighInfoVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName = s"neighbor_fragCommId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype = IR_IntegerDatatype
  override def resolveDefValue = Some(-1)
}

case class NeighborRemoteRank(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends NeighInfoVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)

  override def resolveName = s"neighbor_remoteRank" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype = IR_IntegerDatatype
  override def resolveDefValue = Some("MPI_PROC_NULL")
}

