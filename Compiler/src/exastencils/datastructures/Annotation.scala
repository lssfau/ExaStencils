package exastencils.datastructures

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

/**
  * Holds information and be attached to any class that is [[exastencils.datastructures.Annotatable]].
  *
  * @param id A key-like identifier denoting the kind of information this annotation holds.
  * @param value The information this annotation holds.
  */
sealed class Annotation(val id : String, var value : Any) {
  /**
    * Alternate constructor when no value is needed. Sets the value of this Annotation to None.
    *
    * @param id A key-like identifier denoting the kind of information this annotation holds.
    */
  def this(id : String) = this(id, None)

  /**
    * Re-Sets the value of this annotation.
    *
    * @param newValue The new information this annotation holds.
    */
  def setValue(newValue : Any) = { value = newValue }

  override def toString = { f"Annotation: $id: $value" }
}

/** Companion object for the [[exastencils.datastructures.Annotation]] class. */
object Annotation {
  /**
    * Creates a new [[exastencils.datastructures.Annotation]].
    *
    * @param id A key-like identifier denoting the kind of information this [[exastencils.datastructures.Annotation]] holds.
    * @param value The information this [[exastencils.datastructures.Annotation]] holds.
    * @return The newly created [[exastencils.datastructures.Annotation]] instance.
    */
  def apply(id : String, value : Any) = new Annotation(id, value)

  /**
    * Creates a new [[exastencils.datastructures.Annotation]] when no value is needed. Sets the value of this [[exastencils.datastructures.Annotation]] to None.
    *
    * @param id A key-like identifier denoting the kind of information this [[exastencils.datastructures.Annotation]] holds.
    * @return The newly created [[exastencils.datastructures.Annotation]] instance.
    */
  def apply(id : String) = new Annotation(id)

  /**
    * Splits an [[exastencils.datastructures.Annotation]] into a standard Scala key-value-pair.
    *
    * @param annotation The [[exastencils.datastructures.Annotation]] to be split up.
    * @return A standard Scala key-value-pair, or None.
    */
  def unapply(annotation : Annotation) : Option[(String, Any)] = Some((annotation.id, annotation.value))
}

/** Marks a class as annotatable, meaning [[exastencils.datastructures.Annotation]]s holding metadata can be added. */
trait Annotatable {
  private val annotations_ = new HashMap[String, Annotation]

  /**
    * Adds a [[exastencils.datastructures.Annotation]] to this instance.
    *
    * @param annotation The [[exastencils.datastructures.Annotation]] to add to this instance.
    */
  def add(annotation : Annotation) : Unit = { annotations_ += ((annotation.id, annotation)) }

  /**
    * Adds a list of [[exastencils.datastructures.Annotation]]s to this instance.
    *
    * @param annotations The list of [[exastencils.datastructures.Annotation]]s to add to this instance.
    */
  def add(annotations : Seq[Annotation]) : Unit = { annotations.foreach(add(_)) }

  /**
    * Adds a [[exastencils.datastructures.Annotation]] to this instance.
    *
    * @param annotation The [[exastencils.datastructures.Annotation]] to add to this instance.
    */
  def annotate(annotation : Annotation) : Unit = { annotations_ += ((annotation.id, annotation)) }

  /**
    * Adds a new [[exastencils.datastructures.Annotation]] to this instance.
    *
    * @param id A key-like identifier denoting the kind of information this [[exastencils.datastructures.Annotation]] holds.
    * @param value The information this [[exastencils.datastructures.Annotation]] holds.
    */
  def annotate(id : String, value : Any) = this.add(new Annotation(id, value))

  /**
    * Adds a new [[exastencils.datastructures.Annotation]] to this instance when no value is needed. Sets the value of the new [[exastencils.datastructures.Annotation]] to None.
    *
    * @param id A key-like identifier denoting the kind of information this [[exastencils.datastructures.Annotation]] holds.
    */
  def annotate(id : String) = this.add(new Annotation(id))

  /**
    * Removes a [[exastencils.datastructures.Annotation]] from this instance.
    *
    * @param annotation The [[exastencils.datastructures.Annotation]] to remove.
    */
  def remove(annotation : Annotation) = { annotations_.remove(annotation.id) }

  /**
    * Removes a [[exastencils.datastructures.Annotation]] from this instance.
    *
    * @param annotation The [[exastencils.datastructures.Annotation]] to remove.
    */
  def removeAnnotation(annotation : Annotation) = { annotations_.remove(annotation.id) }

  /**
    * Removes a [[exastencils.datastructures.Annotation]] from this instance.
    *
    * @param id A key-like identifier denoting the [[exastencils.datastructures.Annotation]] to remove.
    */
  def removeAnnotation(id : String) = { annotations_.remove(id) }

  /**
    * Returns all [[exastencils.datastructures.Annotation]]s from this instance.
    */
  def getAnnotations = { annotations_.values }

  /**
    * Returns a [[exastencils.datastructures.Annotation]]s from this instance.
    *
    * @param id A key-like identifier denoting the [[exastencils.datastructures.Annotation]] to return.
    * @return The [[exastencils.datastructures.Annotation]] matching the given identifier, or None.
    */
  def getAnnotation(id : String) = { annotations_.get(id) }

  /**
    * Checks if this instance contains a certain [[exastencils.datastructures.Annotation]].
    *
    * @param id A key-like identifier denoting the [[exastencils.datastructures.Annotation]] to check for.
    * @return true, if the [[exastencils.datastructures.Annotation]] was found, or false.
    */
  def hasAnnotation(id : String) = { annotations_.contains(id) }
}
