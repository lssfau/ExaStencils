package exastencils.datastructures

object Duplicate {
  def apply[T <: Duplicable] (t : T) : T = {
    var duplicate = t.duplicate
    if(t.isInstanceOf[Annotatable]) duplicate.asInstanceOf[Annotatable].add(t.asInstanceOf[Annotatable].getAnnotations)
    duplicate
  }
}

trait Duplicable {
  def duplicate : this.type
}
