package exastencils.knowledge.l3

import scala.collection.mutable._
import scala.reflect.runtime.universe._

import exastencils.knowledge.l4._
import exastencils.logger._

/// L3_KnowledgeCollection

class L3_KnowledgeCollection[L3_Type <: L3_KnowledgeObject[L4_Type] : TypeTag, L4_Type <: L4_KnowledgeObject[_]] {
  var objects : ListBuffer[L3_Type] = ListBuffer()

  def exists(identifier : String) = objects.exists(f => f.name == identifier)

  def getByIdentifier(identifier : String, suppressError : Boolean = false) : Option[L3_Type] = {
    val ret = objects.find(f => f.name == identifier)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[L3_Type].toString } $identifier was not found")
    ret
  }

  def sortedObjects = objects.sortBy(_.name)

  def add(newObj : L3_Type) = objects += newObj

  def clear() = objects.clear()
}

class L3_LeveledKnowledgeCollection[L3_Type <: L3_LeveledKnowledgeObject[L4_Type] : TypeTag, L4_Type <: L4_KnowledgeObject[_]] {
  var objects : ListBuffer[L3_Type] = ListBuffer()

  def exists(identifier : String) = { objects.exists(f => f.name == identifier) }
  def exists(identifier : String, level : Int) = { objects.exists(f => f.name == identifier && f.level == level) }

  def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[L3_Type] = {
    val ret = objects.find(f => f.name == identifier && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[L3_Type].toString } $identifier for level $level was not found")
    ret
  }

  def getAllByIdentifier(identifier : String, suppressError : Boolean = false) : ListBuffer[L3_Type] = {
    var foundObjs = ListBuffer[L3_Type]()
    for (obj <- objects)
      if (obj.name == identifier)
        foundObjs += obj

    if (!suppressError && foundObjs.isEmpty) Logger.warn(s"${ typeOf[L3_Type].toString } $identifier was not found on any level")
    foundObjs
  }

  def sortedObjects = objects.sortBy(obj => (obj.name, obj.level))

  def add(newObj : L3_Type) = { objects += newObj }

  def clear() = objects.clear()
}
