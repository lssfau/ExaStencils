package exastencils.datastructures

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

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
  def apply[T <: Duplicable, U](t : Map[U, T]) : Map[U, T] = {
    t.map(Duplicate(_))
  }
    def apply[T <: Duplicable, U](t : (U, T)) : (U, T) = {
    (t._1, Duplicate(t._2))
  }
}

trait Duplicable {
  def duplicate : this.type
}
