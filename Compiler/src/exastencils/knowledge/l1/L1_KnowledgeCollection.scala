package exastencils.knowledge.l1

import scala.collection.mutable._
import scala.reflect.runtime.universe._

import exastencils.base.l1._
import exastencils.knowledge.l2.L2_KnowledgeObject
import exastencils.logger._

/// L1_KnowledgeCollection

abstract class L1_KnowledgeCollection {
  def name : String
  def length : Int
  def progress() : Unit
  def clear() : Unit
}

/// L1_BasicKnowledgeCollection

abstract class L1_BasicKnowledgeCollection[L1_Type <: L1_KnowledgeObject[L2_Type] : TypeTag, L2_Type <: L2_KnowledgeObject[_]] extends L1_KnowledgeCollection {
  var objects : ListBuffer[L1_Type] = ListBuffer()
  var declared : ListBuffer[String] = ListBuffer()

  def exists(identifier : String) = objects.exists(f => f.name == identifier)
  def existsDecl(identifier : String) = declared.contains(identifier)

  def getByIdentifier(identifier : String, suppressError : Boolean = false) : Option[L1_Type] = {
    val ret = objects.find(f => f.name == identifier)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[L1_Type].toString } $identifier was not found")
    ret
  }

  def sortedObjects = objects.sortBy(_.name)

  def add(newObj : L1_Type) = {
    addDeclared(newObj.name)
    objects += newObj
  }

  def addDeclared(name : String) = declared += name

  override def length = objects.length

  override def clear() = {
    objects.clear()
    declared.clear()
  }
}

/// L1_LeveledKnowledgeCollection

abstract class L1_LeveledKnowledgeCollection[L1_Type <: L1_LeveledKnowledgeObject[L2_Type] : TypeTag, L2_Type <: L2_KnowledgeObject[_]] extends L1_KnowledgeCollection {

  case class NameAndLevel(name : String, level : Int)

  var objects : ListBuffer[L1_Type] = ListBuffer()
  var declared : ListBuffer[NameAndLevel] = ListBuffer()

  def exists(identifier : String) = { objects.exists(_.name == identifier) }
  def exists(identifier : String, level : Int) = { objects.exists(f => f.name == identifier && f.level == level) }

  def existsDecl(identifier : String) = { declared.exists(_.name == identifier) }
  def existsDecl(identifier : String, level : Int) = { declared.exists(f => f.name == identifier && f.level == level) }

  def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[L1_Type] = {
    val ret = objects.find(f => f.name == identifier && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[L1_Type].toString } $identifier for level $level was not found")
    ret
  }

  def getAllByIdentifier(identifier : String, suppressError : Boolean = false) : ListBuffer[L1_Type] = {
    var foundObjs = ListBuffer[L1_Type]()
    for (obj <- objects)
      if (obj.name == identifier)
        foundObjs += obj

    if (!suppressError && foundObjs.isEmpty) Logger.warn(s"${ typeOf[L1_Type].toString } $identifier was not found on any level")
    foundObjs
  }

  def sortedObjects = objects.sortBy(obj => (obj.name, obj.level))

  def add(newObj : L1_Type) = {
    addDeclared(newObj.name, newObj.level)
    objects += newObj
  }

  def addDeclared(name : String, level : Int) : Unit = { declared += NameAndLevel(name, level) }
  def addDeclared(name : String, level : Option[L1_LevelSpecification]) : Unit = {
    if (level.isEmpty) Logger.error(s"Missing level specification for $name")
    level.get match {
      case L1_SingleLevel(lvl) => addDeclared(name, lvl)
      case other               => Logger.error(s"Unsupported level specification for $name: ${ other.prettyprint() }")
    }
  }

  override def length = objects.length

  override def clear() = {
    objects.clear()
    declared.clear()
  }
}
