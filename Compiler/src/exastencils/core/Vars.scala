package exastencils.core

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.language.existentials
import scala.reflect.ClassTag

import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.logger._

object Vars {
  protected var cache = new HashMap[Class[_ <: AnyRef], List[java.lang.reflect.Field]]
  protected val setterSuffix = "_$eq"
  protected val excludeList = List()

  def reset() : Unit = cache.clear()

  def apply[T](o : AnyRef) : List[java.lang.reflect.Field] = {
    cache.getOrElseUpdate(o.getClass(), {
      val vars = gettt(o.getClass).filterNot(p => p.getName().endsWith("$$annotations_")).filterNot(p => p.getName().endsWith("MODULE$"))
      Logger.info(s"""StateManager::Vars: Caching ${vars.length} members of class "${o.getClass.getName()}"""")
      Logger.info(s"""StateManager::Vars: "${o.getClass.getName()}": ${vars}""")
      vars
    })
  }

  def get[T](o : AnyRef, method : java.lang.reflect.Method) : AnyRef = {
    method.invoke(o)
  }

  def get[T](o : AnyRef, field : java.lang.reflect.Field) : AnyRef = {
    field.setAccessible(true)
    field.get(o)
  }

  def set[T](o : AnyRef, method : java.lang.reflect.Method, value : AnyRef) : Boolean = {
    Logger.info(s"Statemananger::set: $o, " + method.getName() + s" to $value")
    try {
      method.invoke(o, value.asInstanceOf[AnyRef])
    } catch {
      case e : Exception => Logger.error(s"""Error setting ${o.toString()}.${method.getName} to '${value}'""")
    }
    true
  }

  def set[T](o : AnyRef, field : java.lang.reflect.Field, value : AnyRef) : Boolean = {
    Logger.info(s"Statemananger::set: $o, " + field.getName() + s" to $value")
    try {
      field.set(o, value)
    } catch {
      case e : Exception => Logger.error(s"""Error setting ${o.toString()}.${field.getName} to '${value}'""")
    }
    true
  }

  protected def gettt(o : Class[_]) : List[java.lang.reflect.Field] = {
    var list = ListBuffer[java.lang.reflect.Field]()
    o.getInterfaces foreach (x => list.++=(gettt(x)))
    if (o.getSuperclass != null) list.++=(gettt(o.getSuperclass))
    list ++= o.getDeclaredFields.toList
    list.toList
  }
}
