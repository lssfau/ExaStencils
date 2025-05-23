package exastencils.waLBerla.ir.communication

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.prettyprinting.PpStream

/// IR_WaLBerlaMPIRequest

case class IR_WaLBerlaMPIRequest(
    var field : IR_FieldLike,
    var send : Boolean,
    var neighIdx : IR_Expression,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaCommVariable {

  override def name : String = s"wbMpiRequest_${ direction }_${ concurrencyId }"

  override def isPrivate : Boolean = true

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def baseDatatype : IR_Datatype = "MPI_Request"
}

/// IR_WaLBerlaMPIRequestNoField

case class IR_WaLBerlaMPIRequestNoField(
    var send : Boolean,
    var neighIdx : IR_Expression,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_WaLBerlaCommVariable {

  override def name : String = s"wbMpiRequestNoField_$direction"

  override def isPrivate : Boolean = true

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, neighIdx)

  override def baseDatatype = "MPI_Request"
}

/// IR_WaLBerlaRemoteReqOutstanding

case class IR_WaLBerlaRemoteReqOutstanding(
    var field : IR_FieldLike,
    var send : Boolean,
    var neighIdx : IR_Expression,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaCommVariable {

  override def name : String = s"wbRemoteReqOutstanding_${ direction }_${ concurrencyId }"

  override def isPrivate : Boolean = true

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def baseDatatype = IR_BooleanDatatype

  override def resolveDefValue() = Some(false)
}

/// IR_WaLBerlaRemoteReqOutstandingNoField

case class IR_WaLBerlaRemoteReqOutstandingNoField(
    var send : Boolean,
    var neighIdx : IR_Expression,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_WaLBerlaCommVariable {

  override def name : String = s"wbRemoteReqOutstandingNoField_${ direction }_${ concurrencyId }"
  override def isPrivate : Boolean = true

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, neighIdx)

  override def baseDatatype = IR_BooleanDatatype

  override def resolveDefValue() = Some(false)
}
