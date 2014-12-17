package exastencils.datastructures.l3

import exastencils.datastructures._

/** Types */
sealed class ScType {
  def toDcType : l4.Datatype = {
    throw new Exception("This type has no L4 equivalence.")
  }
}

case class IntegerDatatype() extends ScType
case class RealDatatype() extends ScType
case class StringDatatype() extends ScType
case class UnitDatatype() extends ScType
case class ArrayDatatype(val elementType: ScType, val numElements: Int) extends ScType
case class ComplexDatatype(val baseType: ScType) extends ScType
case class FieldDatatype() extends ScType
case class StencilDatatype() extends ScType

