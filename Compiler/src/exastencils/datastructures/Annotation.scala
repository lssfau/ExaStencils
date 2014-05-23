package exastencils.datastructures

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

sealed class Annotation(val id : String, var value : Option[Any]) {
  def this(n : String) = this(n, None)

  def setValue(newV : Any) = { value = Some(newV) }
  override def toString = { f"$id: $value" }
}

object Annotation {
  def apply(n : String, v : Option[Any]) = new Annotation(n, v)
  def apply(n : String) = new Annotation(n)
  def unapply(a : Annotation) : Option[(String, Option[Any])] = Some((a.id, a.value))
}

trait Annotatable {
  private val annotations_ = new HashMap[String, Annotation]

  def add(a : Annotation) :Unit  = { annotations_ += ((a.id, a)) }
  def add(as : Seq[Annotation]) :Unit = { as.foreach(add(_)) }
  def annotate(n : String, v : Option[Any]) = this.add(new Annotation(n, v))
  def annotate(n : String) = this.add(new Annotation(n))
  def remove(a : Annotation) = { annotations_.remove(a.id) }
  def removeAnnotation(id : String) = { annotations_.remove(id) }
  def getAnnotations = { annotations_.values }
  def getAnnotation(id : String) = { annotations_.get(id) }
  def hasAnnotation(id : String) = { annotations_.contains(id) }
}

object AnnotationManager {
  protected var names = new HashMap[String, Any => Boolean]

  def allow(name : String) = names.put(name, (x : Any) => true)
  def allow(name : String, checker : (Any => Boolean)) = names.put(name, checker)
  def disallow(name : String) = names.-=(name)
  def valid(name : String, value : Any) : Boolean = names.getOrElse(name, return false).apply(value)
}

