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

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir._
import exastencils.field.ir.IR_FieldCollection
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.logger.Logger

/// IR_VF_NodePositionAsVec

object IR_VF_NodePositionAsVec {
  def find(level : Int) = IR_VirtualField.findVirtualField(s"vf_nodePosition", level).asInstanceOf[IR_VF_NodePositionAsVec]
  def access(level : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level), index)
}

case class IR_VF_NodePositionAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_VirtualFieldWithVec {

  override def name = "vf_nodePosition"
  override def knownAliases = ListBuffer("vf_nodePositionAsVec", "vf_nodePosAsVec", "vf_nodePos")
  override def localization = IR_AtNode
  override def resolutionPossible = true

  override def createDuplicate() = IR_VF_NodePositionAsVec(level, domain)

  def associatedField = {
    if (!Knowledge.grid_isAxisAligned)
      IR_FieldCollection.getByIdentifier(name, level).get
    else
      Logger.error("Trying to access associated field for IR_VF_NodePositionAsVec; not found")
  }

  override def listPerDim = (0 until numDims).map(IR_VF_NodePositionPerDim.find(level, _) : IR_VirtualField).to[ListBuffer]

  override def generateInitCode() = {
    val stmts = ListBuffer[IR_Statement]()

    if (!Knowledge.grid_isAxisAligned) {
      Knowledge.grid_spacingModel match {
        case "uniform"         => stmts ++= IR_SetupNodePositions.for_nonAA_Uniform(level)
        case "random"          => stmts ++= IR_SetupNodePositions.for_nonAA_Random(level)
        case "blockstructured" => // grid is set up in IR_InitDomainFromFile
      }
    }

    stmts
  }

  override def generateInitCodeDependsOn() = {
    if (!Knowledge.grid_isAxisAligned && "random" == Knowledge.grid_spacingModel && level != Knowledge.maxLevel)
      ListBuffer(IR_VF_NodePositionAsVec.find(level + 1))
    else
      ListBuffer()
  }
}

/// IR_VF_NodePositionPerDim

object IR_VF_NodePositionPerDim {
  def find(level : Int, dim : Int) = IR_VirtualField.findVirtualField(s"vf_nodePosition_$dim", level).asInstanceOf[IR_VF_NodePositionPerDim]
  def access(level : Int, dim : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level, dim), index)
}

case class IR_VF_NodePositionPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_VirtualFieldPerDim {

  override def name = s"vf_nodePosition_$dim"
  override def knownAliases = ListBuffer(s"vf_nodePosition_${ IR_Localization.dimToString(dim) }", s"vf_nodePos_$dim", s"vf_nodePos_${ IR_Localization.dimToString(dim) }")
  override def localization = IR_AtNode
  override def resolutionPossible = true

  override def createDuplicate() = IR_VF_NodePositionPerDim(level, domain, dim)

  def associatedField = {
    if (!Knowledge.grid_isUniform)
      IR_FieldCollection.getByIdentifier(name, level).get
    else
      Logger.error("Trying to access associated field for IR_VF_NodePositionPerDim; not found")
  }

  override def resolve(index : IR_ExpressionIndex) : IR_Expression = {
    if (Knowledge.grid_isUniform)
      index(dim) * IR_VF_CellWidthPerDim.access(level, dim, Duplicate(index)) + IR_IV_FragmentPositionBegin(dim)
    else if (Knowledge.grid_isAxisAligned)
      IR_FieldLikeAccess(associatedField, 0, IR_GridUtil.projectIdx(index, dim))
    else {
      var indices : Array[IR_Index] = Array()
      indices :+= IR_ConstIndex(dim)
      indices :+= IR_ConstIndex(0)
      IR_FieldLikeAccess.applySpecial(IR_VF_NodePositionAsVec.find(level).associatedField, 0, index, Some(IR_MatIndex(indices)))
    }
  }

  override def generateInitCode() = {
    val stmts = ListBuffer[IR_Statement]()

    if (Knowledge.grid_isAxisAligned && !Knowledge.grid_isUniform) {
      Knowledge.grid_spacingModel match {
        case "uniform" => stmts ++= IR_SetupNodePositions.for_AA_Uniform(level, dim)

        // setup only on fine level, afterwards restriction
        case "linearFct" if level == Knowledge.maxLevel => stmts ++= IR_SetupNodePositions.for_AA_LinearFct(level, dim)
        case "diego" if level == Knowledge.maxLevel     => stmts ++= IR_SetupNodePositions.for_AA_Diego(level, dim)
        case "diego2" if level == Knowledge.maxLevel    => stmts ++= IR_SetupNodePositions.for_AA_Diego2(level, dim)
        case "diego" | "diego2" | "linearFct"           => stmts ++= IR_SetupNodePositions.for_AA_restrictFromFiner(level, dim)
        case "blockstructured"                          => // grid is set up in IR_InitDomainFromFile
      }
    }

    stmts
  }

  override def generateInitCodeDependsOn() = {
    if (Knowledge.grid_isAxisAligned && !Knowledge.grid_isUniform && "uniform" != Knowledge.grid_spacingModel && level != Knowledge.maxLevel)
      ListBuffer(IR_VF_NodePositionPerDim.find(level + 1, dim))
    else
      ListBuffer()
  }
}
