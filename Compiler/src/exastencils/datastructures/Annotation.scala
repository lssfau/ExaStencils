package exastencils.datastructures

import scala.collection.mutable.ListBuffer

class Annotation(n : String, var v : Option[Any]) {
  def name = n
  def value = v

  def this(n : String) = this(n, None)
  
  def setValue(newV : Any) = { v = Some(newV) }
  override def toString = { f"$name: $value" }
}

trait Annotatable {
  private val annotations_ = new ListBuffer[Annotation]

  def add(a : Annotation) = { annotations_ += a }
  def add(a : Seq[Annotation]) = { annotations_ ++= a }
  def remove(a : Annotation) = { annotations_ -= a }
  def removeAnnotation(name : String) = { if (hasAnnotation(name)) remove(getAnnotation(name).get) }
  def getAnnotations = { annotations_.readOnly }
  def getAnnotation(name : String) = { annotations_.find(p => p.name == name) }
  def hasAnnotation(name : String) = { annotations_.exists(p => p.name == name) }
}
