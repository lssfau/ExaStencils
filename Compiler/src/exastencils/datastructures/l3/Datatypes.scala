package exastencils.datastructures.l3

import exastencils.datastructures._

/** Types */
sealed class ScType {
  def toTcType : l4.Datatype = {
    throw new Exception("This type has no L4 equivalence.")
  }

  def isStatic : Boolean = false
}

case class IntegerDatatype() extends ScType {
  override def toTcType : l4.Datatype = l4.IntegerDatatype()
}
case class RealDatatype() extends ScType {
  override def toTcType : l4.Datatype = l4.RealDatatype()
}
case class StringDatatype() extends ScType {
  override def toTcType : l4.Datatype = l4.StringDatatype()
}
case class UnitDatatype() extends ScType {
  override def toTcType : l4.Datatype = l4.UnitDatatype()
}
case class ArrayDatatype(val elementType : ScType, val numElements : Int) extends ScType {
  override def toTcType : l4.Datatype = l4.ArrayDatatype(elementType.toTcType, numElements)
}
case class ComplexDatatype(val baseType : ScType) extends ScType {
  override def toTcType : l4.Datatype = l4.ComplexDatatype(baseType.toTcType)
}

/* # Static types */

/** Values of static types are only available at compile time. */
sealed class StaticScType() extends ScType {
  override def isStatic : Boolean = true
}

case class FieldDatatype() extends StaticScType
case class StencilDatatype() extends StaticScType

