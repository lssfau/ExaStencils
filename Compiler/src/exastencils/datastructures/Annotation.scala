//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.datastructures

import scala.collection.mutable.HashMap

import exastencils.core._

/** Marks a class as annotatable, meaning annotations holding metadata can be added. */
trait Annotatable {
  private val annotations_ = new HashMap[String, Any]

  /**
    * Adds a new annotation to this instance.
    *
    * @param id    A key-like identifier denoting the kind of information this annotation holds.
    * @param value The information this annotation holds.
    */
  def annotate(id : String, value : Any) : Unit = {
    annotations_ += ((id, value))
  }

  /**
    * Adds a new annotation to this instance and recursively to all its children.
    *
    * @param id    A key-like identifier denoting the kind of information this annotation holds.
    * @param value The information this annotation holds.
    */
  def annotateRecursively(id : String, value : Any) : Unit = {
    this.annotate(id, value)
    Vars(this).foreach(Vars.get(this, _) match {
      case x : Annotatable => x.annotateRecursively(id, value)
      case _               =>
    })
  }

  /**
    * Adds a new annotation without a value to this instance.
    *
    * Sets the value of the new annotation to None.
    *
    * @param id A key-like identifier denoting the kind of information this annotation holds.
    */
  def annotate(id : String) : Unit = this.annotate(id, None)

  /**
    * Adds a new annotation without a value to this instance and recursively to all its children.
    *
    * Sets the value of the new annotation to None.
    *
    * @param id A key-like identifier denoting the kind of information this annotation holds.
    */
  def annotateRecursively(id : String) : Unit = {
    this.annotate(id)
    Vars(this).foreach(Vars.get(this, _) match {
      case x : Annotatable => x.annotateRecursively(id)
      case _               =>
    })
  }

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
    * Copies the annotations from another object to this instance and recursively to all its children.
    *
    * @param other The other object holding the annotations to add to this instance.
    */
  def annotateRecursively(other : Annotatable) : Unit = {
    this.annotate(other)
    Vars(this).foreach(Vars.get(this, _) match {
      case x : Annotatable => x.annotateRecursively(other)
      case _               =>
    })
  }

  /**
    * Removes a annotation from this instance.
    *
    * @param id A key-like identifier denoting the annotation to remove.
    */
  def removeAnnotation(id : String) = { annotations_.remove(id) }

  /**
    * Removes a annotation from this instance and recursively from all its children.
    *
    * @param id A key-like identifier denoting the annotation to remove.
    */
  def removeAnnotationRecursively(id : String) : Unit = {
    this.removeAnnotation(id)
    Vars(this).foreach(Vars.get(this, _) match {
      case x : Annotatable => x.removeAnnotationRecursively(id)
      case _               =>
    })
  }

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
  def getAnnotation(id : String) : Option[Any] = { annotations_.get(id) }

  /**
    * Returns a annotations from this instance.
    *
    * @param id A key-like identifier denoting the annotation to return.
    * @return The annotation matching the given identifier.
    */
  def getAnnotationAs[T](id : String) : T = { annotations_(id).asInstanceOf[T] }

  /**
    * Removes and returns a annotations from this instance.
    *
    * @param id A key-like identifier denoting the annotation to remove and return.
    * @return The annotation matching the given identifier, or `None`.
    */
  def popAnnotation(id : String) : Option[Any] = { val ret = annotations_.get(id); annotations_.remove(id); ret }

  /**
    * Removes and returns a annotations from this instance.
    *
    * @param id A key-like identifier denoting the annotation to remove and return.
    * @return The annotation matching the given identifier.
    */
  def popAnnotationAs[T](id : String) : T = { val ret = annotations_(id).asInstanceOf[T]; annotations_.remove(id); ret }

  /**
    * Checks if this instance contains a certain annotation.
    *
    * @param id A key-like identifier denoting the annotation to check for.
    * @return `true`, if the annotation was found, or `false`.
    */
  def hasAnnotation(id : String) = { annotations_.contains(id) }
}
