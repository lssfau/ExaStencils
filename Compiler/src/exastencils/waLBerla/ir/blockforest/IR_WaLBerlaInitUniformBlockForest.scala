package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_DomainCollection
import exastencils.domain.ir.IR_DomainFromAABB
import exastencils.logger.Logger
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldCollection
import exastencils.waLBerla.ir.grid.IR_WaLBerlaAABB
import exastencils.waLBerla.ir.interfacing._
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_StructuredBlockForest
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_UintType

trait IR_WaLBerlaInitBlockForest extends IR_WaLBerlaWrapperFunction {

  // helper functions
  def datatype = IR_SharedPointerDatatype(WB_StructuredBlockForest)
  def toUnsignedInt(value : Int) = IR_Cast(WB_UintType, value)

  // error checks
  def checkErrors() = {
    if (IR_WaLBerlaFieldCollection.objects.nonEmpty) {
      // assumes all top level fields have the same number of cells
      if (IR_WaLBerlaFieldCollection.objects.filter(_.level == level).forall(f => (0 until numDims).map(d => f.layout.layoutsPerDim(d).numInnerLayers != numCellsBlock(d)).reduce(_ || _)))
        Logger.error("IR_InitBlockForest: Top-level waLBerla fields must have the same size.")

      if (numDims > maxDims) {
        Logger.error("Invalid dimensionality for waLBerla coupling: " + numDims)
      }
    }
  }

  // domain
  def domain = IR_DomainCollection.getByIdentifier("global").get
  def domainBounds = domain.asInstanceOf[IR_DomainFromAABB].aabb

  // helper field (proxy)
  def someWaLBerlaField = IR_WaLBerlaBlockForest().maxLevelWaLBerlaField

  // config
  def maxDims = 3
  def numDims = Knowledge.dimensionality
  def level = if (someWaLBerlaField.isDefined) someWaLBerlaField.get.level else Knowledge.maxLevel
  def arbitraryThickness = 0.01 // dimensionality < 3 -> use one cell with random width for other dims

  // cells (and their width) per block
  def numCellsBlock(d : Int) = if (d < numDims) {
    if (someWaLBerlaField.isDefined) someWaLBerlaField.get.layout.layoutsPerDim(d).numInnerLayers else Knowledge.domain_fragmentLengthAsVec(d) * (1 << level)
  } else
    1
  def cellWidth(d : Int) = if (d < numDims) (domainBounds.upper(d) - domainBounds.lower(d)) / (Knowledge.domain_rect_numFragsTotalAsVec(d) * numCellsBlock(d)) else arbitraryThickness

  // partitioning
  def wbBlocks = List(Knowledge.domain_rect_numFragsTotal_x, Knowledge.domain_rect_numFragsTotal_y, Knowledge.domain_rect_numFragsTotal_z).map(toUnsignedInt)
  def numProcesses = List(Knowledge.domain_rect_numBlocks_x, Knowledge.domain_rect_numBlocks_y, Knowledge.domain_rect_numBlocks_z).map(toUnsignedInt)
  def periodicity = List(Knowledge.domain_rect_periodic_x, Knowledge.domain_rect_periodic_y, Knowledge.domain_rect_periodic_z)

  // aabb
  def aabbLower = (0 until maxDims).map(d => IR_RealConstant(if (d < numDims) domainBounds.lower(d) else 0.0))
  def aabbUpper = (0 until maxDims).map(d => IR_RealConstant(if (d < numDims) domainBounds.upper(d) else cellWidth(d)))

  // function modifiers
  override def isInterfaceFunction : Boolean = false
  override def inlineIncludeImplementation : Boolean = false
}

case class IR_WaLBerlaInitUniformBlockForest() extends IR_WaLBerlaInitBlockForest {

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {

    // check for errors
    checkErrors()

    // compile body
    var body : ListBuffer[IR_Statement] = ListBuffer()

    val aabb = IR_VariableAccess("aabb", IR_WaLBerlaAABB.datatype)
    body += IR_VariableDeclaration(aabb, IR_FunctionCall(IR_ExternalFunctionReference("math::AABB"), aabbLower ++ aabbUpper : _*))

    body += IR_Return(
      new IR_FunctionCall(IR_ExternalFunctionReference("blockforest::createUniformBlockGrid"),
        ListBuffer(
          aabb,
          wbBlocks(0), wbBlocks(1), wbBlocks(2),
          numCellsBlock(0), numCellsBlock(1), numCellsBlock(2),
          numProcesses(0), numProcesses(1), numProcesses(2),
          periodicity(0), periodicity(1), periodicity(2))))

    IR_WaLBerlaPlainFunction(name, datatype, ListBuffer(), body)
  }

  override def name : String = "createUniformBlockForest"
}
