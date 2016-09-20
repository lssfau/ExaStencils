package exastencils.knowledge.l4

import scala.collection.mutable._
import scala.reflect.runtime.universe._

import exastencils.logger._

class L4_KnowledgeCollection[T <: L4_KnowledgeObjectWithIdent : TypeTag] {
  var objects : ListBuffer[T] = ListBuffer()

  def exists(identifier : String) = objects.exists(f => f.identifier == identifier)

  def getByIdentifier(identifier : String, suppressError : Boolean = false) : Option[T] = {
    val ret = objects.find(f => f.identifier == identifier)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[T].toString } $identifier was not found")
    ret
  }

  def add(newObj : T) = objects += newObj
}

class L4_LeveledKnowledgeCollection[T <: L4_KnowledgeObjectWithIdentAndLevel : TypeTag] {
  var objects : ListBuffer[T] = ListBuffer()

  def exists(identifier : String) = { objects.exists(f => f.identifier == identifier) }
  def exists(identifier : String, level : Int) = { objects.exists(f => f.identifier == identifier && f.level == level) }

  def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[T] = {
    val ret = objects.find(f => f.identifier == identifier && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[T].toString } $identifier for level $level was not found")
    ret
  }

  def getAllByIdentifier(identifier : String, suppressError : Boolean = false) : ListBuffer[T] = {
    var foundObjs = ListBuffer[T]()
    for (obj <- objects)
      if (obj.identifier == identifier)
        foundObjs += obj

    if (!suppressError && foundObjs.isEmpty) Logger.warn(s"${ typeOf[T].toString } $identifier was not found on any level")
    foundObjs
  }

  def add(newObj : T) = { objects += newObj }
}
