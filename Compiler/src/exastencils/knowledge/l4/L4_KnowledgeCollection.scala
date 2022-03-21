//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.knowledge.l4

import scala.collection.mutable._
import scala.reflect.runtime.universe._

import exastencils.base.l4._
import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.logger._

/// L4_KnowledgeCollection

abstract class L4_KnowledgeCollection {
  def name : String
  def length : Int
  def progress() : Unit
  def clear() : Unit
}

/// L4_BasicKnowledgeCollection

abstract class L4_BasicKnowledgeCollection[L4_Type <: L4_KnowledgeObject[IR_Type] : TypeTag, IR_Type <: IR_KnowledgeObject] extends L4_KnowledgeCollection {
  var objects : ListBuffer[L4_Type] = ListBuffer()
  var declared : ListBuffer[String] = ListBuffer()

  def exists(identifier : String) = objects.exists(f => f.name == identifier)
  def existsDecl(identifier : String) = declared.contains(identifier)

  def getByIdentifier(identifier : String, suppressError : Boolean = false) : Option[L4_Type] = {
    val ret = objects.find(f => f.name == identifier)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[L4_Type].toString } $identifier was not found")
    ret
  }

  def sortedObjects = objects.sortBy(_.name)

  def add(newObj : L4_Type) = {
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

/// L4_LeveledKnowledgeCollection

abstract class L4_LeveledKnowledgeCollection[L4_Type <: L4_LeveledKnowledgeObject[IR_Type] : TypeTag, IR_Type <: IR_KnowledgeObject] extends L4_KnowledgeCollection {

  case class NameAndLevel(name : String, level : Int)

  var objects : ListBuffer[L4_Type] = ListBuffer()
  var declared : ListBuffer[NameAndLevel] = ListBuffer()

  def exists(identifier : String) = { objects.exists(_.name == identifier) }
  def exists(identifier : String, level : Int) = { objects.exists(f => f.name == identifier && f.level == level) }

  def existsDecl(identifier : String) : Boolean = { declared.exists(_.name == identifier) }
  def existsDecl(identifier : String, level : Int) : Boolean = { declared.exists(f => f.name == identifier && f.level == level) }
  def existsDecl(identifier : String, level : Option[L4_LevelSpecification]) : Boolean = {
    if (level.isEmpty) Logger.error(s"Missing level specification for $identifier")
    level.get match {
      case L4_SingleLevel(lvl) => existsDecl(identifier, lvl)
      case other               => Logger.error(s"Unsupported level specification for $identifier: ${ other.prettyprint() }")
    }
  }

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

  def add(newObj : L4_Type) = {
    addDeclared(newObj.name, newObj.level)
    objects += newObj
  }

  def addDeclared(identifier : String, level : Int) : Unit = { declared += NameAndLevel(identifier, level) }
  def addDeclared(identifier : String, level : Option[L4_LevelSpecification]) : Unit = {
    if (level.isEmpty) Logger.error(s"Missing level specification for $identifier")
    level.get match {
      case L4_SingleLevel(lvl) => addDeclared(identifier, lvl)
      case other               => Logger.error(s"Unsupported level specification for $identifier: ${ other.prettyprint() }")
    }
  }

  override def length = objects.length

  override def clear() = {
    objects.clear()
    declared.clear()
  }
}
