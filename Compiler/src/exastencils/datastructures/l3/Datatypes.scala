package exastencils.datastructures.l3

import exastencils.datastructures._

/** Types */
sealed class ScType {
  def toDcType : l4.Datatype = {
    throw new Exception("This type has no L4 equivalence.")
  }

  def isStatic : Boolean = false
}

/** Values of static types are only available at compile time. */
sealed class StaticScType() extends ScType {
  override def isStatic : Boolean = true
}

case class IntegerDatatype() extends ScType
case class RealDatatype() extends ScType
case class StringDatatype() extends ScType
case class UnitDatatype() extends ScType
case class ArrayDatatype(val elementType : ScType, val numElements : Int) extends ScType
case class ComplexDatatype(val baseType : ScType) extends ScType
case class FieldDatatype() extends StaticScType
case class StencilDatatype() extends StaticScType

