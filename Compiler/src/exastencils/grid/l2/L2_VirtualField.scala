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

import exastencils.base.l2._
import exastencils.baseExt.l2._
import exastencils.domain.l2.L2_Domain
import exastencils.grid.l3.L3_VirtualField
import exastencils.knowledge.l2.L2_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_VirtualField

object L2_VirtualField {
  def findVirtualField(name : String, level : Int) = L2_VirtualFieldCollection.getByIdentifier(name, level).get
}

trait L2_VirtualField extends L2_LeveledKnowledgeObject[L3_VirtualField] {
  def knownAliases : ListBuffer[String]
  def datatype : L2_Datatype
  def domain : L2_Domain
  def localization : L2_Localization
  def resolutionPossible : Boolean

  def numDims = domain.numDims

  override def prettyprintDecl(out : PpStream) = Logger.error("Trying to print the declaration of a virtual field - unsupported")
}

/// L2_VirtualFieldWithVec

trait L2_VirtualFieldWithVec extends L2_VirtualField {
  override def datatype = L2_VectorDatatype(L2_RealDatatype, numDims)

  def listPerDim : ListBuffer[L2_VirtualField]
}

/// L2_VirtualFieldWithScalar

trait L2_VirtualFieldWithScalar extends L2_VirtualField {
  override def datatype = L2_RealDatatype

  def resolve(index : L2_ExpressionIndex) : L2_Expression
}

/// L2_VirtualFieldPerDim

trait L2_VirtualFieldPerDim extends L2_VirtualFieldWithScalar {
  def dim : Int
}
