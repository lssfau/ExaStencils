package exastencils.knowledge.l4

import scala.collection.mutable._
import scala.reflect.runtime.universe._

import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.logger._
import exastencils.prettyprinting.PpStream

trait L4_KnowledgeObject {
  def prettyprintDecl(out : PpStream) : Unit
  def progress : IR_KnowledgeObject
  def getProgressedObject : IR_KnowledgeObject
}

trait L4_HasIdentifier extends L4_KnowledgeObject {
  def identifier : String
}

trait L4_HasLevel extends L4_HasIdentifier {
  def level : Int
}

trait L4_HasIdentifierAndLevel extends L4_HasIdentifier with L4_HasLevel

class L4_KnowledgeCollection[T <: L4_HasIdentifier : TypeTag] {
  var objects : ListBuffer[T] = ListBuffer()

  def exists(identifier : String) = objects.exists(f => f.identifier == identifier)

  def getByIdentifier(identifier : String, suppressError : Boolean = false) : Option[T] = {
    val ret = objects.find(f => f.identifier == identifier)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[T].toString } $identifier was not found")
    ret
  }

  def add(newObj : T) = objects += newObj
}

class L4_LeveledKnowledgeCollection[T <: L4_HasIdentifierAndLevel : TypeTag] {
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
