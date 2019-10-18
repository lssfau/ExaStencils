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

package exastencils.knowledge.ir

import scala.collection.mutable._
import scala.reflect.runtime.universe._

import exastencils.logger._

class IR_KnowledgeCollection[T <: IR_KnowledgeObject : TypeTag] {
  var objects : ListBuffer[T] = ListBuffer()

  def exists(identifier : String) = objects.exists(f => f.name == identifier)

  def getByIdentifier(identifier : String, suppressError : Boolean = false) : Option[T] = {
    val ret = objects.find(f => f.name == identifier)
    if (!suppressError && ret.isEmpty) Logger.warn(s"${ typeOf[T].toString } $identifier was not found")
    ret
  }

  def sortedObjects = objects.sortBy(_.name)

  def add(newObj : T) = objects += newObj

  def clear() = objects.clear()
}

class IR_LeveledKnowledgeCollection[T <: IR_LeveledKnowledgeObject : TypeTag] {
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

  def clear() = objects.clear()
}
