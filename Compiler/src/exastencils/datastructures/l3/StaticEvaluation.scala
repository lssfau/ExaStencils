package exastencils.datastructures.l3

import TcbImplicits._
import exastencils.datastructures.l4

/** Static values. */
trait StaticValue extends Value

/** Static locations */
trait StaticLValue extends StaticValue with LValue {

  def writeTcAssignment(block : TcbBlock, tcRhs : DynamicRValue)

}

case class IntegerLValue(tcId : String) extends StaticLValue {

  override def scType = IntegerDatatype()

  def writeTcAssignment(block : TcbBlock, tcRhs : DynamicRValue) {
    val tcAccess = new l4.BasicAccess(tcId)
    block += l4.AssignmentStatement(tcAccess, tcRhs.tcExpression, "=")
  }

  override def deref = ???
}
case class FieldLValue(tcId : String) extends StaticLValue {

  override def scType = FieldDatatype()

  override def writeTcAssignment(block : TcbBlock, tcRhs : DynamicRValue) {

    val tcAccess = new l4.FieldAccess(
      tcId,
      l4.CurrentLevelSpecification(),
      l4.IntegerConstant(0),
      -1) // FIXME@Christian array index

    block += l4.AssignmentStatement(tcAccess, tcRhs.tcExpression, "=")

  }

  override def deref = FieldRValue(tcId)
}

/* =================================================================== */

/** Static values */
trait StaticRValue extends StaticValue with RValue

case class IntegerRValue(v : Int) extends StaticRValue {
  override def scType = IntegerDatatype()
}
case class StencilRValue() extends StaticRValue {
  override def scType = StencilDatatype()
}

case class FieldRValue(tcId : String) extends StaticRValue {

  override def scType = FieldDatatype()

  def toTc() : l4.Expression = {
    l4.FieldAccess(
      tcId,
      l4.CurrentLevelSpecification(),
      l4.IntegerConstant(0),
      -1) // FIXME@Christian array index
  }
}
