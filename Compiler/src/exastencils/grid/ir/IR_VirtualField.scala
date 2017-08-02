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
