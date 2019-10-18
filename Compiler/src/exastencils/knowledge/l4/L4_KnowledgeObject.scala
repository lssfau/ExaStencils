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

import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// L4_KnowledgeObject

object L4_KnowledgeObject {
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[L4_KnowledgeObject[_]])
}

trait L4_KnowledgeObject[IR_Equivalent <: IR_KnowledgeObject] {

  L4_KnowledgeObject

  def name : String
  def progressImpl() : IR_Equivalent
  def prettyprintDecl(out : PpStream) : Unit
  def createDuplicate() : L4_KnowledgeObject[IR_Equivalent] = ???

  private var progressed : Option[IR_Equivalent] = None

  final def progress() : IR_Equivalent = {
    // progress if not already done - otherwise simply return the progressed object
    if (progressed.isEmpty)
      progressed = Some(progressImpl())
    progressed.get
  }

  // alias for progress -> progress object if not already progressed
  def getProgressedObj() : IR_Equivalent = progress()
}

/// L4_LeveledKnowledgeObject

trait L4_LeveledKnowledgeObject[IR_Equivalent <: IR_KnowledgeObject] extends L4_KnowledgeObject[IR_Equivalent] {
  def level : Int
}
