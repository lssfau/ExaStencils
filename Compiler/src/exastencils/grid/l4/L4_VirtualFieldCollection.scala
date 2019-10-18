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

package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.grid.ir._
import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._
import exastencils.logger.Logger

/// L4_VirtualFieldCollection

object L4_VirtualFieldCollection extends L4_LeveledKnowledgeCollection[L4_VirtualField, IR_VirtualField] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_PrepareVirtualFieldDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessVirtualFieldDeclarations

  L4_PrepareAccesses.strategies += L4_PrepareVirtualFieldAccesses
  L4_ResolveAccesses.strategies += L4_ResolveVirtualFieldAccesses

  override def name = "L4_VirtualFieldCollection"
  override def progress() = objects.foreach(obj => IR_VirtualFieldCollection.add(obj.progress()))

  // special overrides for handling possible name variations

  def prefixedLC(identifier : String) = (if (identifier.startsWith("vf_")) identifier else "vf_" + identifier).toLowerCase

  override def exists(identifier : String) = objects.exists(_.name.toLowerCase == prefixedLC(identifier))
  override def exists(identifier : String, level : Int) = objects.exists(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)

  override def existsDecl(identifier : String) = declared.exists(_.name.toLowerCase == prefixedLC(identifier))
  override def existsDecl(identifier : String, level : Int) = declared.exists(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)

  override def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[L4_VirtualField] = {
    val ret = objects.find(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"L4_VirtualField $identifier for level $level was not found")
    ret
  }

  override def getAllByIdentifier(identifier : String, suppressError : Boolean = false) : ListBuffer[L4_VirtualField] = {
    var foundObjs = ListBuffer[L4_VirtualField]()
    for (obj <- objects)
      if (obj.name.toLowerCase == prefixedLC(identifier))
        foundObjs += obj

    if (!suppressError && foundObjs.isEmpty) Logger.warn(s"L4_VirtualField $identifier was not found on any level")
    foundObjs
  }
}
