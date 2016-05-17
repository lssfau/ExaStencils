package exastencils.datastructures

import scala.collection.mutable.HashMap
import exastencils.core.Duplicate

/** Marks a class as annotatable, meaning annotations holding metadata can be added. */
trait Annotatable {
  private val annotations_ = new HashMap[String, Any]

  /**
    * Adds a new annotation to this instance.
    *
    * @param id A key-like identifier denoting the kind of information this annotation holds.
    * @param value The information this annotation holds.
    */
  def annotate(id : String, value : Any) : Unit = {
    annotations_ += ((id, value))
  }

  /**
    * Adds a new annotation to this instance when no value is needed.
    *
    * Sets the value of the new annotation to None.
    *
    * @param id A key-like identifier denoting the kind of information this annotation holds.
    */
  def annotate(id : String) : Unit = this.annotate(id, None)

  /**
    * Copies the annotations from another object.
    *
    * @param other The other object holding the annotations to add to this instance.
    */
  def annotate(other : Annotatable) : Unit = {
    if (other ne this) {
      var z = other.annotations_
      annotations_ ++= Duplicate(z)
    }
  }

  /**
    * Removes a annotation from this instance.
    *
    * @param id A key-like identifier denoting the annotation to remove.
    */
  def removeAnnotation(id : String) = { annotations_.remove(id) }

  /**
    * Returns all annotations of this instance.
    *
    * @return The list of annotation of this instance.
    */
  def annotations = annotations_

  /**
    * Returns a annotations from this instance.
    *
    * @param id A key-like identifier denoting the annotation to return.
    * @return The annotation matching the given identifier, or `None`.
    */
  def getAnnotation(id : String) = { annotations_.get(id) }

  /**
    * Checks if this instance contains a certain annotation.
    *
    * @param id A key-like identifier denoting the annotation to check for.
    * @return `true`, if the annotation was found, or `false`.
    */
  def hasAnnotation(id : String) = { annotations_.contains(id) }
}
