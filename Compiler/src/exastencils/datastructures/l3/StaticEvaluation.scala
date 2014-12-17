package exastencils.datastructures.l3

/** Static values. */
sealed class StaticValue

/** Static locations */
abstract sealed class LValue extends StaticValue
case class IntegerLValue(val num: Integer)

/** Static values */
abstract sealed class RValue extends StaticValue
case class IntegerRValue(v: Int) extends RValue
case class StencilRValue() extends RValue

