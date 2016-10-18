package exastencils.knowledge.l2

import scala.collection.mutable._
import scala.reflect.runtime.universe._

import exastencils.logger._

/// L2_KnowledgeCollection

class L2_KnowledgeCollection[T <: L2_KnowledgeObject : TypeTag] {
  var objects : ListBuffer[T] = ListBuffer()

  def exists(identifier : String) = objects.exists(f => f.name == identifier)

  def getByIdentifier(identifier : String, suppressError : Boolean = false) : Option[T] = {
    val ret = objects.find(f => f.name == identifier)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[T].toString } $identifier was not found")
    ret
  }

  def sortedObjects = objects.sortBy(_.name)

  def add(newObj : T) = objects += newObj
}

class L2_LeveledKnowledgeCollection[T <: L2_KnowledgeObjectWithLevel : TypeTag] {
  var objects : ListBuffer[T] = ListBuffer()

  def exists(identifier : String) = { objects.exists(f => f.name == identifier) }
  def exists(identifier : String, level : Int) = { objects.exists(f => f.name == identifier && f.level == level) }

  def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[T] = {
    val ret = objects.find(f => f.name == identifier && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[T].toString } $identifier for level $level was not found")
    ret
  }

  def getAllByIdentifier(identifier : String, suppressError : Boolean = false) : ListBuffer[T] = {
    var foundObjs = ListBuffer[T]()
    for (obj <- objects)
      if (obj.name == identifier)
        foundObjs += obj

    if (!suppressError && foundObjs.isEmpty) Logger.warn(s"${ typeOf[T].toString } $identifier was not found on any level")
    foundObjs
  }

  def sortedObjects = objects.sortBy(obj => (obj.name, obj.level))

  def add(newObj : T) = { objects += newObj }
}
