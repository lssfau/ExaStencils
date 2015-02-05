/** Classes for managing the internal representation of stencils. */
package exastencils.datastructures.l3

import exastencils.datastructures.l4

object StencilOffset {
  def apply(c : StaticValue) : StencilOffset = {
    c match {
      case ListStaticValue(el) =>
        new StencilOffset(el map {
          case StaticInteger(v) => v
          case _                => throw new Exception("Stencil offsets need to be integers.")
        })
    }
  }
}

class StencilOffset(coordinates : List[Int]) {
  def dimension = coordinates.length

  def toTc() : l4.ExpressionIndex = {
    /** @todo Implement general n-D case. */
    coordinates match {
      case List(x, y) =>
        l4.ExpressionIndex2D(l4.IntegerConstant(x), l4.IntegerConstant(y))
      case List(x, y, z) =>
        l4.ExpressionIndex3D(l4.IntegerConstant(x), l4.IntegerConstant(y), l4.IntegerConstant(z))
      case _ =>
        throw new Exception("Target language does not support %d dimensions.".format(dimension))
    }
  }

  def toSc() = ListStaticValue(coordinates map { StaticInteger(_) })

  def isZero = coordinates forall { _ == 0 }
}

object StencilEntry {
  def apply(e : StaticValue) : StencilEntry = {
    e match {
      case ListStaticValue(List(offset, StaticReal(value))) =>
        new StencilEntry(StencilOffset(offset), value)
      case _ =>
        throw new Exception("Stencil entry needs to be a list and a float.")
    }
  }
}
class StencilEntry(val offset : StencilOffset, val value : Double) {

  def toTc() : l4.StencilEntry = {
    l4.StencilEntry(offset.toTc(), l4.FloatConstant(value))
  }

  def toSc() = {
    ListStaticValue(List(offset.toSc(), StaticReal(value)))
  }

  def invertValue() = new StencilEntry(offset, 1 / value)

  def atZero = offset.isZero
}

object Stencil {
  def apply(s : ListStaticValue) : Stencil = {
    val entries = s.elements map { e => StencilEntry(e) }
    new Stencil(entries.toList)
  }
}
/**
  * Manages logical data structure for stencils.
  *  It can generate target and source code.
  */
class Stencil(val entries : List[StencilEntry]) {

  // always validate
  validate()

  def isValid : Boolean = {
    entries forall { e => e.offset.dimension == dimension }
  }
  def validate() {
    if (!isValid) {
      throw new Exception("Stencil is not valid. (Dimension mismatch?)")
    }
  }

  def dimension = entries.head.offset.dimension

  def toTc(id : String) = {

    l4.StencilDeclarationStatement(
      l4.LeveledIdentifier(id,
        l4.AllLevelsSpecification()),
      entries map { _.toTc() })
  }

  def toSc() = {
    ListStaticValue(entries map { _.toSc() })
  }

  def diagInv() : Stencil = {
    new Stencil((entries filter { _.atZero }) map { _.invertValue() })
  }
}
