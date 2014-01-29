package exastencils.datastructures

import java.lang.reflect._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

object Duplicate {
  def apply[T <: Duplicable](t : T) : T = {
    var duplicate = t.duplicate
    if (t.isInstanceOf[Annotatable]) duplicate.asInstanceOf[Annotatable].add(t.asInstanceOf[Annotatable].getAnnotations)
    duplicate
  }
  def apply[T <: Duplicable](t : List[T]) : List[T] = {
    t.map(Duplicate(_))
  }
  def apply[T <: Duplicable](t : ListBuffer[T]) : ListBuffer[T] = {
    t.map(Duplicate(_))
  }
  def apply[T <: Duplicable, U](t : Map[U, T]) : Map[U, T] = {
    t.map(Duplicate(_))
  }
  def apply[T <: Duplicable, U](t : (U, T)) : (U, T) = {
    (t._1, Duplicate(t._2))
  }
}

trait Duplicable {
  def duplicate : this.type
}

/*

trait Duplicater {
  def duplicate(original : AnyRef, cache : HashMap[AnyRef, AnyRef]) : AnyRef
}

object Duplication {
  var cloneMap = new scala.collection.mutable.HashMap[Class[_], Duplicater]
  cloneMap.put(classOf[AnyRef], new AnyRefDuplicater)

  def duplicate[T <: AnyRef](original : T) = duplicateReflectionInternal(original, new HashMap[AnyRef, AnyRef])

  def getDuplicater(obj : AnyRef) : Duplicater = {
    val classes = getClassHierarchy(obj.getClass, true)
    classes.foreach(c => {
      if (cloneMap.contains(c)) return cloneMap.get(c).get
    })
    return cloneMap.get(obj.getClass).get
  }

  def duplicateReflectionInternal(original : AnyRef, cache : HashMap[AnyRef, AnyRef]) : AnyRef = {
    if (original == null) return original; else if (cache.contains(original)) return cache.get(original).get
    val duplicater = getDuplicater(original)
    duplicater.duplicate(original, new HashMap[AnyRef, AnyRef])
  }

  def getClassHierarchy(clazz : Class[_], includeInterfaces : Boolean) = {
    val classes = new ListBuffer[Class[_]]
    var c = clazz
    while (c != null) {
      classes += c
      if (includeInterfaces) {
        val interfaces = c.getInterfaces
        interfaces.foreach(i => classes += i)
      }
      c = c.getSuperclass
    }
    classes
  }
}

class AnyRefDuplicater extends Duplicater {
  override def duplicate(original : AnyRef, cache : HashMap[AnyRef, AnyRef]) : AnyRef = {
    // FIXME checks if original is immutable

    val c = original.getClass()
    val fields = getFields(c, false) // FIXME
    try {
      val copy = instantiate(original)
      cache.put(original, copy)
      fields.foreach(f => {
        var obj = f.get(original)
        obj = Duplication.duplicateReflectionInternal(obj, cache)
        f.set(copy, obj)
      })
      return copy
    } catch {
      case t : Throwable => exastencils.core.WARN(s"could not duplicate original: $t"); return original
    }
  }

  def instantiate(original : AnyRef) = { //original.getClass.newInstance()
    val ctor = original.getClass.getConstructor()
    // FIXME get needed parameters for constructor, call constructor with parameters
    ctor.newInstance()
  }

  var fieldCache = new scala.collection.mutable.HashMap[String, scala.Array[Field]]

  def getFields(obj : AnyRef, includeStatic : Boolean) : scala.Array[Field] = getFields(obj, includeStatic, true)
  def getFields(obj : AnyRef, includeStatic : Boolean, includeTransient : Boolean) : scala.Array[Field] = getFields(obj.getClass, includeStatic, includeTransient)
  def getFields(c : Class[_], includeStatic : Boolean) : scala.Array[Field] = getFields(c, includeStatic, true)
  def getFields(c : Class[_], includeStatic : Boolean, includeTransient : Boolean) = {
    val cacheKey = c.getCanonicalName() + ":" + includeStatic
    var array = fieldCache.get(cacheKey).getOrElse(null)

    if (array == null) {
      var fields = new ListBuffer[Field]
      var classes = Duplication.getClassHierarchy(c, false).reverse

      classes.foreach(clazz => {
        val allFields = clazz.getDeclaredFields
        allFields.foreach(f => {
          var skipThis = false // FIXME remove this horrible workaround because of a missing continue keyword in Scala
          if ((!includeTransient) && ((f.getModifiers() & Modifier.TRANSIENT) == Modifier.TRANSIENT)) {
            skipThis = true
          } else if (!skipThis && f.isSynthetic()) {
            // Synthetic fields are bad!!!11!cos(0)
            skipThis = true
          }
          if (!skipThis) {
            val isStatic = (f.getModifiers() & Modifier.STATIC) == Modifier.STATIC;
            if ((isStatic) && (!includeStatic)) {
              skipThis = true
            }
            if (!skipThis && f.getName().equalsIgnoreCase("serialVersionUID")) {
              skipThis = true
            }
            if (!skipThis) {
              f.setAccessible(true)
              fields += f
            }
          }
        })
      })

      array = fields.toArray
      fieldCache.put(cacheKey, array)
    }
    array
  }
}
*/
*/
