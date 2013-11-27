package exastencils.datastructures

class Annotation(n : String, var v : Any) {
  def name = n
  def value = v
  
  def setValue(newV : Any) = { v = newV }
  
  override def clone : Annotation = {
     return new Annotation(n, v)
  }
}