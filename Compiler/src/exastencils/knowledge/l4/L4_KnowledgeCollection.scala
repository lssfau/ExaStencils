package exastencils.knowledge.l4

import scala.collection.mutable._
import scala.reflect.runtime.universe._

import exastencils.knowledge.ir._
import exastencils.logger._

/// L4_KnowledgeCollection

class L4_KnowledgeCollection[L4_Type <: L4_KnowledgeObject[IR_Type] : TypeTag, IR_Type <: IR_KnowledgeObject] {
  var objects : ListBuffer[L4_Type] = ListBuffer()

  def exists(identifier : String) = objects.exists(f => f.name == identifier)

  def getByIdentifier(identifier : String, suppressError : Boolean = false) : Option[L4_Type] = {
    val ret = objects.find(f => f.name == identifier)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[L4_Type].toString } $identifier was not found")
    ret
  }

  def sortedObjects = objects.sortBy(_.name)

  def add(newObj : L4_Type) = objects += newObj
}

class L4_LeveledKnowledgeCollection[L4_Type <: L4_KnowledgeObjectWithLevel[IR_Type] : TypeTag, IR_Type <: IR_KnowledgeObject] {
  var objects : ListBuffer[L4_Type] = ListBuffer()

  def exists(identifier : String) = { objects.exists(f => f.name == identifier) }
  def exists(identifier : String, level : Int) = { objects.exists(f => f.name == identifier && f.level == level) }

  def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[L4_Type] = {
    val ret = objects.find(f => f.name == identifier && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[L4_Type].toString } $identifier for level $level was not found")
    ret
  }

  def getAllByIdentifier(identifier : String, suppressError : Boolean = false) : ListBuffer[L4_Type] = {
    var foundObjs = ListBuffer[L4_Type]()
    for (obj <- objects)
      if (obj.name == identifier)
        foundObjs += obj

    if (!suppressError && foundObjs.isEmpty) Logger.warn(s"${ typeOf[L4_Type].toString } $identifier was not found on any level")
    foundObjs
  }

  def sortedObjects = objects.sortBy(obj => (obj.name, obj.level))

  def add(newObj : L4_Type) = { objects += newObj }
}
