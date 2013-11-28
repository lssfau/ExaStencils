package exastencils.datastructures

import scala.collection.mutable.ListBuffer

class Annotation(n : String, var v : Any) {
  def name = n
  def value = v
  
  def setValue(newV : Any) = { v = newV }
  
  override def clone : Annotation = {
     return new Annotation(n, v)
  }
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
