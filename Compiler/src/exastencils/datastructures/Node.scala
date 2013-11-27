package exastencils.datastructures

import scala.collection.mutable.ListBuffer

trait Node extends Annotatable {
	def deepClone : Node
}

trait Annotatable {
  private val annotations_ = new ListBuffer[Annotation]

  def add(a : Annotation) = { annotations_ += a }
  def remove(a : Annotation) = { annotations_ -= a }
  def removeAnnotation(name : String) = { if (hasAnnotation(name)) remove(getAnnotation(name).get) }
  def getAnnotation(name : String) : Option[Annotation] = { return annotations_.find(p => p.name == name) }
  def hasAnnotation(name : String) : Boolean = { return annotations_.exists(p => p.name == name) }
}

