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

package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.grid.l3._
import exastencils.knowledge.l2.L2_KnowledgeContainer._
import exastencils.knowledge.l2._
import exastencils.logger.Logger

/// L2_VirtualFieldCollection

object L2_VirtualFieldCollection extends L2_LeveledKnowledgeCollection[L2_VirtualField, L3_VirtualField] {
  exastencils.core.Duplicate.registerConstant(this)

  L2_KnowledgeContainer.register(this)

  L2_PrepareDeclarations.strategies += L2_PrepareVirtualFieldDeclarations
  L2_ProcessDeclarations.strategies += L2_ProcessVirtualFieldDeclarations

  L2_PrepareAccesses.strategies += L2_PrepareVirtualFieldAccesses
  L2_ResolveAccesses.strategies += L2_ResolveVirtualFieldAccesses

  override def name = "L2_VirtualFieldCollection"
  override def progress() = objects.foreach(obj => L3_VirtualFieldCollection.add(obj.progress()))

  // special overrides for handling possible name variations

  def prefixedLC(identifier : String) = (if (identifier.startsWith("vf_")) identifier else "vf_" + identifier).toLowerCase

  override def exists(identifier : String) = objects.exists(_.name.toLowerCase == prefixedLC(identifier))
  override def exists(identifier : String, level : Int) = objects.exists(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)

  override def existsDecl(identifier : String) = declared.exists(_.name.toLowerCase == prefixedLC(identifier))
  override def existsDecl(identifier : String, level : Int) = declared.exists(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)

  override def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[L2_VirtualField] = {
    val ret = objects.find(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"L2_VirtualField $identifier for level $level was not found")
    ret
  }

  override def getAllByIdentifier(identifier : String, suppressError : Boolean = false) : ListBuffer[L2_VirtualField] = {
    var foundObjs = ListBuffer[L2_VirtualField]()
    for (obj <- objects)
      if (obj.name.toLowerCase == prefixedLC(identifier))
        foundObjs += obj

    if (!suppressError && foundObjs.isEmpty) Logger.warn(s"L2_VirtualField $identifier was not found on any level")
    foundObjs
  }
}
