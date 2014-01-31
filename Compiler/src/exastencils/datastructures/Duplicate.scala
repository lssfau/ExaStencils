package exastencils.datastructures

import java.lang.reflect._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

object Duplicate {
  val cloner = new com.rits.cloning.Cloner

  def apply[T](t : T) : T = cloner.deepClone(t)
    
//  def apply[T <: Duplicable](t : T) : T = {
//    var duplicate = t.duplicate
//    if (t.isInstanceOf[Annotatable]) duplicate.asInstanceOf[Annotatable].add(t.asInstanceOf[Annotatable].getAnnotations)
//    duplicate
//  }
//  def apply[T <: Duplicable](t : List[T]) : List[T] = {
//    t.map(Duplicate(_))
//  }
//  def apply[T <: Duplicable](t : ListBuffer[T]) : ListBuffer[T] = {
//    t.map(Duplicate(_))
//  }
//  def apply[T <: Duplicable, U](t : Map[U, T]) : Map[U, T] = {
//    t.map(Duplicate(_))
//  }
//  def apply[T <: Duplicable, U](t : (U, T)) : (U, T) = {
//    (t._1, Duplicate(t._2))
//  }
}

trait Duplicable {
  def duplicate : this.type
}


//
//trait Duplicater {
//  def duplicate(original : AnyRef, cache : HashMap[AnyRef, AnyRef]) : AnyRef
//}
//
//object Duplication {
//  var cloneMap = new scala.collection.mutable.HashMap[Class[_], Duplicater]
//  cloneMap.put(classOf[scala.collection.mutable.Map[_, _]], new MapDuplicater)
//  cloneMap.put(classOf[AnyRef], new AnyRefDuplicater)
//
//  def duplicate[T <: AnyRef](original : T) = duplicateReflectionInternal(original, new HashMap[AnyRef, AnyRef])
//
//  def getDuplicater(obj : AnyRef) : Duplicater = {
//    val classes = getClassHierarchy(obj.getClass, true)
//    classes.foreach(c => {
//      if (cloneMap.contains(c)) return cloneMap.get(c).get
//    })
//    return cloneMap.get(obj.getClass).get
//  }
//
//  def duplicateReflectionInternal(original : AnyRef, cache : HashMap[AnyRef, AnyRef]) : AnyRef = {
//    if (original == null) return original; else if (cache.contains(original)) return cache.get(original).get
//    val duplicater = getDuplicater(original)
//    duplicater.duplicate(original, new HashMap[AnyRef, AnyRef])
//  }
//
//  def getClassHierarchy(clazz : Class[_], includeInterfaces : Boolean) = {
//    val classes = new ListBuffer[Class[_]]
//    var c = clazz
//    while (c != null) {
//      classes += c
//      if (includeInterfaces) {
//        val interfaces = c.getInterfaces
//        interfaces.foreach(i => classes += i)
//      }
//      c = c.getSuperclass
//    }
//    classes
//  }
//}
//
//class MapDuplicater extends Duplicater {
////  override def duplicate(original : AnyRef, cache: HashMap[AnyRef, AnyRef]) : AnyRef = {
////    val clone = original.getClass.getMethods().find(p => p.getName == "clone")
////    val m = clone.get
////    m.invoke(original)
////  }
//  override def duplicate(original : AnyRef, cache: HashMap[AnyRef, AnyRef]) : AnyRef = {
////  Map clone = (Map)ObjectCloner.instantiate(original);
////                
////                // Populate data
////                for (Object entry : ((Map)original).entrySet()) {
////                        clone.put(CloneUtilities.deepCloneReflectionInternal(((Entry)entry).getKey(), cache), CloneUtilities.deepCloneReflectionInternal(((Entry)entry).getValue(), cache));
////                }
////                
////                return clone;
//    val arefdup = Duplication.getDuplicater(AnyRef).asInstanceOf[AnyRefDuplicater]
//    var clone = arefdup.instantiate(original)
//    original.asInstanceOf[Map[_, _]].foreach(entry => {
//      val key = Duplication.duplicateReflectionInternal(entry._1.asInstanceOf[AnyRef], cache)
//      val value = Duplication.duplicateReflectionInternal(entry._2.asInstanceOf[AnyRef], cache)
//      clone.getClass.getMethods().find(p => p.getName == "put").get.invoke(clone, key, value)
//    })
//    clone
//  }
//}
//
//class AnyRefDuplicater extends Duplicater {
//  override def duplicate(original : AnyRef, cache : HashMap[AnyRef, AnyRef]) : AnyRef = {
//    // check if original is immutable
//    original match {
//      case _ : Number => return original
//      case _ : String => return original
//      case _ : Character => return original
//      case _ => 
//    }
//
//    val c = original.getClass()
//    val fields = getFields(c, false) // FIXME
//    try {
////      val copyMethod = original.getClass.getMethods().find(p => p.getName == "copy")//original.getClass().getMethod("copy")
////      println(s"copymethod: $copyMethod")
//      val copy = instantiate(original)
//      cache.put(original, copy)
//      fields.foreach(f => {
//        var obj = f.get(original)
//        obj = Duplication.duplicateReflectionInternal(obj, cache)
//        f.set(copy, obj)
//      })
//      return copy
//    } catch {
//      case t : Throwable => exastencils.core.WARN(s"could not duplicate original: ${t.printStackTrace()}"); return original
//    }
//  }
//
//
//  
//  
//  def instantiate(original : AnyRef) : AnyRef = { //original.getClass.newInstance()
//    if(original.getClass.getConstructors.length <= 0) {
//      println(s"no constructor: $original")
//      return original
//    }
//    
//    val ctor = original.getClass.getConstructors.head
//    // FIXME get needed parameters for constructor, call constructor with parameters
//    
//    //println(ctor.getParameterTypes())
//    
//    val f = original.getClass().getDeclaredFields()
//    f.foreach(_.setAccessible(true))
//    
//    println(s"original: $original; fields: ${f.length}")
//    
//    //ctor.getParameterTypes().foreach(x => println("arg:" + x))
//    for(i <- 0 to ctor.getParameterTypes.length-1) {
//      println(s"arg$i: ${ctor.getParameterTypes()(i)} <-> ${f(i).get(original).asInstanceOf[AnyRef]} ")
//    }
//    
//    ctor.getParameterTypes().length match {
//      case 0 => println("c0");ctor.newInstance().asInstanceOf[AnyRef]
//      case 1 => println("c1");ctor.newInstance(f(0).get(original)).asInstanceOf[AnyRef]
//      case 2 => println("c2");ctor.newInstance(f(0).get(original), f(1).get(original)).asInstanceOf[AnyRef]
//    }
//    
//    //ctor.newInstance(new scala.collection.mutable.HashMap[String, Node]).asInstanceOf[AnyRef]
//    //original.getClass.newInstance
//  }
//
//  var fieldCache = new scala.collection.mutable.HashMap[String, scala.Array[Field]]
//
//  def getFields(obj : AnyRef, includeStatic : Boolean) : scala.Array[Field] = getFields(obj, includeStatic, true)
//  def getFields(obj : AnyRef, includeStatic : Boolean, includeTransient : Boolean) : scala.Array[Field] = getFields(obj.getClass, includeStatic, includeTransient)
//  def getFields(c : Class[_], includeStatic : Boolean) : scala.Array[Field] = getFields(c, includeStatic, true)
//  def getFields(c : Class[_], includeStatic : Boolean, includeTransient : Boolean) = {
//    val cacheKey = c.getCanonicalName() + ":" + includeStatic
//    var array = fieldCache.get(cacheKey).getOrElse(null)
//
//    if (array == null) {
//      var fields = new ListBuffer[Field]
//      var classes = Duplication.getClassHierarchy(c, false).reverse
//
//      classes.foreach(clazz => {
//        val allFields = clazz.getDeclaredFields
//        allFields.foreach(f => {
//          var skipThis = false // FIXME remove this horrible workaround because of a missing continue keyword in Scala
//          if ((!includeTransient) && ((f.getModifiers() & Modifier.TRANSIENT) == Modifier.TRANSIENT)) {
//            skipThis = true
//          } else if (!skipThis && f.isSynthetic()) {
//            // Synthetic fields are bad!!!11!cos(0)
//            skipThis = true
//          }
//          if (!skipThis) {
//            val isStatic = (f.getModifiers() & Modifier.STATIC) == Modifier.STATIC;
//            if ((isStatic) && (!includeStatic)) {
//              skipThis = true
//            }
//            if (!skipThis && f.getName().equalsIgnoreCase("serialVersionUID")) {
//              skipThis = true
//            }
//            if (!skipThis) {
//              f.setAccessible(true)
//              fields += f
//            }
//          }
//        })
//      })
//
//      array = fields.toArray
//      fieldCache.put(cacheKey, array)
//    }
//    array
//  }
//}

