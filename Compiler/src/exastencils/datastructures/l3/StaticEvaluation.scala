package exastencils.datastructures.l3

import TcbImplicits._
import exastencils.datastructures.l4

/** Static values. */
abstract sealed class StaticValue(val scType : ScType)

/** Static locations */
abstract sealed class LValue(scType : ScType) extends StaticValue(scType) {

  def writeTcAssignment(block : TcbBlock, tcRhs : DynamicRValue)

}

case class IntegerLValue(tcId : String) extends LValue(IntegerDatatype()) {
  def writeTcAssignment(block : TcbBlock, tcRhs : DynamicRValue) {

    val tcAccess = new l4.BasicAccess(tcId)
    block += l4.AssignmentStatement(tcAccess, tcRhs.tcExpression, "=")
  }
}
case class FieldLValue(tcId : String) extends LValue(FieldDatatype()) {

  override def writeTcAssignment(block : TcbBlock, tcRhs : DynamicRValue) {

    val tcAccess = new l4.FieldAccess(
      tcId,
      l4.CurrentLevelSpecification(),
      l4.IntegerConstant(0),
      -1) // FIXME@Christian array index

    block += l4.AssignmentStatement(tcAccess, tcRhs.tcExpression, "=")

  }
}

/** Static values */
abstract sealed class RValue(scType : ScType) extends StaticValue(scType)
case class IntegerRValue(v : Int) extends RValue(IntegerDatatype())
case class StencilRValue() extends RValue(StencilDatatype())

