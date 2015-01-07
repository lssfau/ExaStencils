/** Classes for managing the internal representation of stencils. */
package exastencils.datastructures.l3

import exastencils.datastructures.l4
import scala.collection.mutable.ListBuffer

object StencilOffset {
  def apply(c : StaticValue) : StencilOffset = {

    c match {
      case StaticListRValue(el) =>
        new StencilOffset(el map {
          case IntegerRValue(v) => v
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
}

object StencilEntry {
  def apply(e : StaticValue) : StencilEntry = {
    e match {
      case StaticListRValue(List(offset, FloatRValue(value))) =>
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

}

object Stencil {
  def apply(s : StaticListRValue) : Stencil = {
    val entries = s.elements map { e => StencilEntry(e) }
    new Stencil(entries)
  }
}
/** Manages logical data structure for stencils. */
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

}

/** Manages the code for all automatically generated stencils. */
class StencilManager {

  // add a stencil and return its identifier
  private val stms = ListBuffer[l4.SpecialStatement]()
  def statements = stms

  private var idCount = 0

  def genId() : String = {
    idCount += 1
    "__stencil_id%02d".format(idCount)
  }

  def add(s : StaticListRValue) : String = {

    val id = genId()

    val stencil = Stencil(s)
    stms += stencil.toTc(id)

    id
  }

}
