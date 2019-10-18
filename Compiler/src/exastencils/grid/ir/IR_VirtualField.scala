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

package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.domain.ir.IR_Domain
import exastencils.knowledge.ir.IR_LeveledKnowledgeObject

/// IR_VirtualField

object IR_VirtualField {
  def findVirtualField(name : String, level : Int) = IR_VirtualFieldCollection.getByIdentifier(name, level).get
}

trait IR_VirtualField extends IR_LeveledKnowledgeObject {
  def knownAliases : ListBuffer[String]
  def datatype : IR_Datatype
  def domain : IR_Domain
  def localization : IR_Localization
  def resolutionPossible : Boolean

  def numDims = domain.numDims

  def generateInitCode() : ListBuffer[IR_Statement] = ListBuffer()
  def generateInitCodeDependsOn() : ListBuffer[IR_VirtualField] = ListBuffer()
}

/// IR_VirtualFieldWithVec

trait IR_VirtualFieldWithVec extends IR_VirtualField {
  override def datatype = IR_MatrixDatatype(IR_RealDatatype, numDims, 1)

  def listPerDim : ListBuffer[IR_VirtualField]
}

/// IR_VirtualFieldWithScalar

trait IR_VirtualFieldWithScalar extends IR_VirtualField {
  override def datatype = IR_RealDatatype

  def resolve(index : IR_ExpressionIndex) : IR_Expression
}

/// IR_VirtualFieldPerDim

trait IR_VirtualFieldPerDim extends IR_VirtualFieldWithScalar {
  def dim : Int
}
