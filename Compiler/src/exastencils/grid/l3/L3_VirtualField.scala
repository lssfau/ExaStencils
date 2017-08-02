package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.baseExt.l3.L3_MatrixDatatype
import exastencils.domain.l3.L3_Domain
import exastencils.grid.l4.L4_VirtualField
import exastencils.knowledge.l3.L3_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_VirtualField

object L3_VirtualField {
  def findVirtualField(name : String, level : Int) = L3_VirtualFieldCollection.getByIdentifier(name, level).get
}

trait L3_VirtualField extends L3_LeveledKnowledgeObject[L4_VirtualField] {
  def knownAliases : ListBuffer[String]
  def datatype : L3_Datatype
  def domain : L3_Domain
  def localization : L3_Localization
  def resolutionPossible : Boolean

  def numDims = domain.numDims

  override def prettyprintDecl(out : PpStream) = Logger.error("Trying to print the declaration of a virtual field - unsupported")
}

/// L3_VirtualFieldWithVec

trait L3_VirtualFieldWithVec extends L3_VirtualField {
  override def datatype = L3_MatrixDatatype(L3_RealDatatype, numDims, 1)

  def listPerDim : ListBuffer[L3_VirtualField]
}

/// L3_VirtualFieldWithScalar

trait L3_VirtualFieldWithScalar extends L3_VirtualField {
  override def datatype = L3_RealDatatype

  def resolve(index : L3_ExpressionIndex) : L3_Expression
}

/// L3_VirtualFieldPerDim

trait L3_VirtualFieldPerDim extends L3_VirtualFieldWithScalar {
  def dim : Int
}
