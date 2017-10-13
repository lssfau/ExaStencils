package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_MatrixDatatype
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
  override def datatype = L4_MatrixDatatype(L4_RealDatatype, numDims, 1)

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
