package exastencils.datastructures.l3

/** Static values. */
abstract sealed class StaticValue(val scType : ScType)

/** Static locations */
abstract sealed class LValue(scType : ScType) extends StaticValue(scType)
case class IntegerLValue(val num : Integer) extends LValue(IntegerDatatype())
case class FieldLValue(tcId : String) extends LValue(FieldDatatype())

/** Static values */
abstract sealed class RValue(scType : ScType) extends StaticValue(scType)
case class IntegerRValue(v : Int) extends RValue(IntegerDatatype())
case class StencilRValue() extends RValue(StencilDatatype())

