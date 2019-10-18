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

package exastencils.knowledge.l2

import exastencils.knowledge.l3.L3_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// L2_KnowledgeObject

object L2_KnowledgeObject {
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[L2_KnowledgeObject[_]])
}

trait L2_KnowledgeObject[L3_Equivalent <: L3_KnowledgeObject[_]] {

  L2_KnowledgeObject

  def name : String
  def progressImpl() : L3_Equivalent
  def prettyprintDecl(out : PpStream) : Unit
  def createDuplicate() : L2_KnowledgeObject[L3_Equivalent]

  private var progressed : Option[L3_Equivalent] = None

  final def progress() : L3_Equivalent = {
    // progress if not already done - otherwise simply return the progressed object
    if (progressed.isEmpty)
      progressed = Some(progressImpl())
    progressed.get
  }

  // alias for progress -> progress object if not already progressed
  def getProgressedObj() : L3_Equivalent = progress()
}

/// L2_LeveledKnowledgeObject

trait L2_LeveledKnowledgeObject[L3_Equivalent <: L3_KnowledgeObject[_]] extends L2_KnowledgeObject[L3_Equivalent] {
  def level : Int
}
