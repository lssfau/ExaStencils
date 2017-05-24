package exastencils.knowledge.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_KnowledgeCollection extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/knowledge/|LAYER_LC|/|LAYER_UC|_KnowledgeCollection.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.knowledge.|LAYER_LC|

import scala.collection.mutable._
import scala.reflect.runtime.universe._

import exastencils.base.|LAYER_LC|._
import exastencils.knowledge.|NEXT_LC|.|NEXT_UC|_KnowledgeObject
import exastencils.logger._

/// |LAYER_UC|_KnowledgeCollection

abstract class |LAYER_UC|_KnowledgeCollection {
  def name : String
  def length : Int
  def progress() : Unit
  def clear() : Unit
}

/// |LAYER_UC|_BasicKnowledgeCollection

abstract class |LAYER_UC|_BasicKnowledgeCollection[|LAYER_UC|_Type <: |LAYER_UC|_KnowledgeObject[|NEXT_UC|_Type] : TypeTag, |NEXT_UC|_Type <: |NEXT_UC|_KnowledgeObject[_]] extends |LAYER_UC|_KnowledgeCollection {
  var objects : ListBuffer[|LAYER_UC|_Type] = ListBuffer()
  var declared : ListBuffer[String] = ListBuffer()

  def exists(identifier : String) = objects.exists(f => f.name == identifier)
  def existsDecl(identifier : String) = declared.contains(identifier)

  def getByIdentifier(identifier : String, suppressError : Boolean = false) : Option[|LAYER_UC|_Type] = {
    val ret = objects.find(f => f.name == identifier)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[|LAYER_UC|_Type].toString } $identifier was not found")
    ret
  }

  def sortedObjects = objects.sortBy(_.name)

  def add(newObj : |LAYER_UC|_Type) = {
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

/// |LAYER_UC|_LeveledKnowledgeCollection

abstract class |LAYER_UC|_LeveledKnowledgeCollection[|LAYER_UC|_Type <: |LAYER_UC|_LeveledKnowledgeObject[|NEXT_UC|_Type] : TypeTag, |NEXT_UC|_Type <: |NEXT_UC|_KnowledgeObject[_]] extends |LAYER_UC|_KnowledgeCollection {

  case class NameAndLevel(name : String, level : Int)

  var objects : ListBuffer[|LAYER_UC|_Type] = ListBuffer()
  var declared : ListBuffer[NameAndLevel] = ListBuffer()

  def exists(identifier : String) = { objects.exists(_.name == identifier) }
  def exists(identifier : String, level : Int) = { objects.exists(f => f.name == identifier && f.level == level) }

  def existsDecl(identifier : String) = { declared.exists(_.name == identifier) }
  def existsDecl(identifier : String, level : Int) = { declared.exists(f => f.name == identifier && f.level == level) }

  def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[|LAYER_UC|_Type] = {
    val ret = objects.find(f => f.name == identifier && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[|LAYER_UC|_Type].toString } $identifier for level $level was not found")
    ret
  }

  def getAllByIdentifier(identifier : String, suppressError : Boolean = false) : ListBuffer[|LAYER_UC|_Type] = {
    var foundObjs = ListBuffer[|LAYER_UC|_Type]()
    for (obj <- objects)
      if (obj.name == identifier)
        foundObjs += obj

    if (!suppressError && foundObjs.isEmpty) Logger.warn(s"${ typeOf[|LAYER_UC|_Type].toString } $identifier was not found on any level")
    foundObjs
  }

  def sortedObjects = objects.sortBy(obj => (obj.name, obj.level))

  def add(newObj : |LAYER_UC|_Type) = {
    addDeclared(newObj.name, newObj.level)
    objects += newObj
  }

  def addDeclared(name : String, level : Int) : Unit = { declared += NameAndLevel(name, level) }
  def addDeclared(name : String, level : Option[|LAYER_UC|_LevelSpecification]) : Unit = {
    if (level.isEmpty) Logger.error(s"Missing level specification for $name")
    level.get match {
      case |LAYER_UC|_SingleLevel(lvl) => addDeclared(name, lvl)
      case other               => Logger.error(s"Unsupported level specification for $name: ${ other.prettyprint() }")
    }
  }

  override def length = objects.length

  override def clear() = {
    objects.clear()
    declared.clear()
  }
}
"""
  }
}
