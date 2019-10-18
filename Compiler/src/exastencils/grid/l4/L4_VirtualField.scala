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

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.domain.l4.L4_Domain
import exastencils.grid.ir.IR_VirtualField
import exastencils.knowledge.l4.L4_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_VirtualField

object L4_VirtualField {
  def findVirtualField(name : String, level : Int) = L4_VirtualFieldCollection.getByIdentifier(name, level).get
}

trait L4_VirtualField extends L4_LeveledKnowledgeObject[IR_VirtualField] {
  def knownAliases : ListBuffer[String]
  def datatype : L4_Datatype
  def domain : L4_Domain
  def localization : L4_Localization
  def resolutionPossible : Boolean

  def numDims = domain.numDims

  override def prettyprintDecl(out : PpStream) = Logger.error("Trying to print the declaration of a virtual field - unsupported")

  def addAdditionalFieldsToKnowledge() : Unit = {}
}

/// L4_VirtualFieldWithVec

trait L4_VirtualFieldWithVec extends L4_VirtualField {
  override def datatype = L4_VectorDatatype(L4_RealDatatype, numDims)

  def listPerDim : ListBuffer[L4_VirtualField]
}

/// L4_VirtualFieldWithScalar

trait L4_VirtualFieldWithScalar extends L4_VirtualField {
  override def datatype = L4_RealDatatype

  def resolve(index : L4_ExpressionIndex) : L4_Expression
}

/// L4_VirtualFieldPerDim

trait L4_VirtualFieldPerDim extends L4_VirtualFieldWithScalar {
  def dim : Int
}
