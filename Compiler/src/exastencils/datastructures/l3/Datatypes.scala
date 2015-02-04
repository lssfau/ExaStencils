package exastencils.datastructures.l3

import exastencils.datastructures._

/** Types */
sealed class ScType {
  def toTcType : l4.Datatype = {
    throw new Exception("This type has no L4 equivalence.")
  }

  def isStatic : Boolean = false

  def createDynamicLocation(ctx : Context) : StaticValue = {
    throw new Exception("This datatype has no dynamic representation.")
  }
}
case class FunctionDatatype() extends ScType

case class NilDatatype() extends ScType
case class LocationDatatype() extends ScType
case class IntegerDatatype() extends ScType {
  override def toTcType : l4.Datatype = l4.IntegerDatatype()
}
case class RealDatatype() extends ScType {
  override def toTcType : l4.Datatype = l4.RealDatatype()

  override def createDynamicLocation(ctx : Context) : StaticValue = {
    new DynamicRealLocation(ctx.genId())
  }
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
case class StaticListDatatype() extends ScType

/* # Static types */

/** Values of static types are only available at compile time. */
sealed class StaticScType() extends ScType {
  override def isStatic : Boolean = true
}

case class FieldDatatype() extends StaticScType
case class StencilDatatype() extends StaticScType

abstract class DynamicLocation extends StaticValue {

}

class DynamicRealLocation(id : String) extends DynamicLocation {
  override def scType() = RealDatatype()
}

class DynamicFieldLocation(id : String) extends DynamicLocation {
  override def scType() = RealDatatype()
}

