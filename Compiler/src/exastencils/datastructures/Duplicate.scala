package exastencils.datastructures

import scala.collection.mutable.ListBuffer

object Duplicate {
  def apply[T <: Duplicable](t : T) : T = {
    var duplicate = t.duplicate
    if (t.isInstanceOf[Annotatable]) duplicate.asInstanceOf[Annotatable].add(t.asInstanceOf[Annotatable].getAnnotations)
    duplicate
  }
  def apply[T <: Duplicable](t : Seq[T]) : List[T] = {
    var duplicateList = new ListBuffer[T]
    t.foreach(t => duplicateList += Duplicate(t))
    duplicateList.readOnly
  }
}

trait Duplicable {
  def duplicate : this.type
}
