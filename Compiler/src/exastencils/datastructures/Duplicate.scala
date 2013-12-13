package exastencils.datastructures

import scala.collection.mutable.ListBuffer

object Duplicate {
  def apply[T <: Duplicable](t : T) : T = {
    var duplicate = t.duplicate
    if (t.isInstanceOf[Annotatable]) duplicate.asInstanceOf[Annotatable].add(t.asInstanceOf[Annotatable].getAnnotations)
    duplicate
  }
  def apply[T <: Duplicable](t : List[T]) : List[T] = {
    t.map(Duplicate(_))
  }
  def apply[T <: Duplicable](t : ListBuffer[T]) : ListBuffer[T] = {
    t.map(Duplicate(_))
  }
}

trait Duplicable {
  def duplicate : this.type
}
