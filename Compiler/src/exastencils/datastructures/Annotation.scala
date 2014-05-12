package exastencils.datastructures

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

class Annotation(n : String, var v : Option[Any]) {
  def name = n
  def value = v

  def this(n : String) = this(n, None)

  def setValue(newV : Any) = { v = Some(newV) }
  override def toString = { f"$name: $value" }
}

object Annotation {
  def apply(n : String, v : Option[Any]) = new Annotation(n, v)
  def apply(n : String) = new Annotation(n)
}

trait Annotatable {
  private val annotations_ = new ListBuffer[Annotation]

  def add(a : Annotation) = { annotations_ += a }
  def add(a : Seq[Annotation]) = { annotations_ ++= a }
  def annotate(n : String, v : Option[Any]) = this.add(new Annotation(n, v))
  def annotate(n : String) = this.add(new Annotation(n))
  def remove(a : Annotation) = { annotations_ -= a }
  def removeAnnotation(name : String) = { if (hasAnnotation(name)) remove(getAnnotation(name).get) }
  def getAnnotations = { annotations_.toList }
  def getAnnotation(name : String) = { annotations_.find(p => p.name == name) }
  def hasAnnotation(name : String) = { annotations_.exists(p => p.name == name) }
}

object AnnotationManager {
  protected var names = new HashMap[String, Any => Boolean]

  def allow(name : String) = names.put(name, (x : Any) => true)
  def allow(name : String, checker : (Any => Boolean)) = names.put(name, checker)
  def disallow(name : String) = names.-=(name)
  def valid(name : String, value : Any) : Boolean = names.getOrElse(name, return false).apply(value)
}

