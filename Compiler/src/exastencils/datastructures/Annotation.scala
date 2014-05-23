package exastencils.datastructures

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

sealed class Annotation(val id : String, var value : Any) {
  def this(id : String) = this(id, None)

  def setValue(newValue : Any) = { value = newValue }
  override def toString = { f"Annotation: $id: $value" }
}

object Annotation {
  def apply(id : String, value : Any) = new Annotation(id, value)
  def apply(id : String) = new Annotation(id)
  def unapply(annotation : Annotation) : Option[(String, Any)] = Some((annotation.id, annotation.value))
}

trait Annotatable {
  private val annotations_ = new HashMap[String, Annotation]

  def add(annotation : Annotation) :Unit  = { annotations_ += ((annotation.id, annotation)) }
  def add(annotations : Seq[Annotation]) :Unit = { annotations.foreach(add(_)) }
  def annotate(annotation : Annotation) :Unit  = { annotations_ += ((annotation.id, annotation)) }
  def annotate(id : String, value : Any) = this.add(new Annotation(id, value))
  def annotate(id : String) = this.add(new Annotation(id))
  def remove(annotation : Annotation) = { annotations_.remove(annotation.id) }
  def removeAnnotation(annotation : Annotation) = { annotations_.remove(annotation.id) }
  def removeAnnotation(id : String) = { annotations_.remove(id) }
  def getAnnotations = { annotations_.values }
  def getAnnotation(id : String) = { annotations_.get(id) }
  def hasAnnotation(id : String) = { annotations_.contains(id) }
}
