package exastencils.datastructures.l3

import TcbImplicits._
import exastencils.datastructures._

/** Types */
sealed class ScType {
  def toTcType : l4.Datatype = {
    throw new Exception("This type has no L4 equivalence.")
  }

  def isStatic : Boolean = false

  def createDynamicLocation(ctx : Context) : DynamicLocation = {
    throw new Exception("This datatype has no dynamic representation.")
  }
}
case class FunctionDatatype() extends ScType

case class NilDatatype() extends ScType
case class LocationDatatype() extends ScType
case class IntegerDatatype() extends ScType {
  override def toTcType : l4.Datatype = l4.IntegerDatatype
}
case class RealDatatype() extends ScType {
  override def toTcType : l4.Datatype = l4.RealDatatype

  override def createDynamicLocation(ctx : Context) : DynamicLocation = {
    new DynamicRealLocation(ctx.genId())
  }
}
case class StringDatatype() extends ScType {
  override def toTcType : l4.Datatype = l4.StringDatatype
}

case class CharDatatype() extends ScType {
  override def toTcType : l4.Datatype = l4.CharDatatype
}

case class UnitDatatype() extends ScType {
  override def toTcType : l4.Datatype = l4.UnitDatatype
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
case class StaticDatatype() extends StaticScType

/**
  * This class contains the target code for obtaining a location.
  *
  *  Thus it contains methods for obtaining or setting a value at runtime.
  */
abstract class DynamicLocation extends StaticValue {

  def writeTcForWriting(block : TcbBlock, rhs : DynamicLocation) {
    ???
  }

  def tcForReading() : l4.Expression = {
    ???
  }

  def +(rhs : DynamicLocation) : DynamicLocation = {
    throw new Exception("Operator '+' is not defined.")
  }
  def -(rhs : DynamicLocation) : DynamicLocation = {
    throw new Exception("Operator '-' is not defined.")
  }
  def *(rhs : DynamicLocation) : DynamicLocation = {
    throw new Exception("Operator '*' is not defined.")
  }

  def argumentTc() : l4.VariableAccess = ???
}

class DynamicRealLocation(id : String) extends DynamicLocation {
  override def scType() = RealDatatype()

  override def *(rhs : DynamicLocation) = {

    rhs match {
      // Scalar * Vector
      case e : DynamicFieldLocationExpression =>
        DynamicFieldLocationExpression(l4.BinaryExpression("*", tcForReading, rhs.tcForReading))
      case f : DynamicFieldLocation =>
        DynamicFieldLocationExpression(l4.BinaryExpression("*", tcForReading, rhs.tcForReading))
    }
  }

  override def tcForReading() : l4.Expression = l4.VariableAccess(id, None, l4.RealDatatype)

  override def argumentTc() : l4.VariableAccess = l4.VariableAccess(id, None, l4.RealDatatype)
}

case class DynamicFieldLocation(tcId : String) extends DynamicLocation {
  override def scType() = FieldDatatype()

  override def +(rhs : DynamicLocation) = {
    new DynamicFieldLocationExpression(l4.BinaryExpression("+", tcForReading, rhs.tcForReading))
  }

  override def -(rhs : DynamicLocation) = {
    new DynamicFieldLocationExpression(l4.BinaryExpression("-", tcForReading, rhs.tcForReading))
  }

  override def *(rhs : DynamicLocation) = {
    new DynamicFieldLocationExpression(l4.BinaryExpression("*", tcForReading, rhs.tcForReading))
  }

  private val tcAccess = new l4.FieldAccess(
    tcId,
    l4.CurrentLevelSpecification,
    l4.SlotModifier.Constant(0))

  override def writeTcForWriting(block : TcbBlock, rhs : DynamicLocation) {

    block += l4.AssignmentStatement(tcAccess, rhs.tcForReading(), "=")
  }

  override def tcForReading() : l4.Expression = tcAccess
}

case class DynamicFieldLocationExpression(val expr : l4.Expression) extends DynamicLocation {
  override def +(rhs : DynamicLocation) = {
    new DynamicFieldLocationExpression(l4.BinaryExpression("+", tcForReading, rhs.tcForReading))
  }

  override def -(rhs : DynamicLocation) = {
    new DynamicFieldLocationExpression(l4.BinaryExpression("-", tcForReading, rhs.tcForReading))
  }

  override def *(rhs : DynamicLocation) = {
    new DynamicFieldLocationExpression(l4.BinaryExpression("*", tcForReading, rhs.tcForReading))
  }

  override def scType() = FieldDatatype()

  override def tcForReading() : l4.Expression = expr
}

case class PrimitiveDynamicLocation(val expr : l4.Expression, val scType : ScType) extends DynamicLocation {

  override def tcForReading() : l4.Expression = expr
}

case class UnitLocation() extends DynamicLocation {
  def scType = UnitDatatype()

  override def tcForReading() : l4.Expression = TcUnit()
}

