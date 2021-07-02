package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Cast
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Return
import exastencils.base.ir.IR_SharedPointerDatatype
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_DomainCollection
import exastencils.domain.ir.IR_DomainFromAABB
import exastencils.logger.Logger
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_StructuredBlockStorage

case class IR_InitBlockForest() extends IR_WaLBerlaFuturePlainFunction {
  def domain = IR_DomainCollection.getByIdentifier("global").get
  def domainBounds = domain.asInstanceOf[IR_DomainFromAABB].aabb
  def datatype = IR_SharedPointerDatatype(WB_StructuredBlockStorage)

  val numDims  = Knowledge.dimensionality
  val level = Knowledge.maxLevel

  def toUnsignedInt(value : Int) = IR_Cast(IR_SpecialDatatype("uint_t"), value)

  def numCellsPerFrag(d : Int) = (1 << level) * Knowledge.domain_fragmentLengthAsVec(d)
  def numCellsTotal(d : Int) = toUnsignedInt( numCellsPerFrag(d) * Knowledge.domain_rect_numFragsTotalAsVec(d) )
  def cellWidth(d : Int) = (domainBounds.upper(d) - domainBounds.lower(d)) / numCellsTotal(d)

  val wbBlocks = List(Knowledge.domain_rect_numFragsTotal_x, Knowledge.domain_rect_numFragsTotal_y, Knowledge.domain_rect_numFragsTotal_z).map(toUnsignedInt)
  val numProcesses = List(Knowledge.domain_rect_numBlocks_x, Knowledge.domain_rect_numBlocks_y, Knowledge.domain_rect_numBlocks_z).map(toUnsignedInt)
  val periodicity = List(Knowledge.domain_rect_periodic_x, Knowledge.domain_rect_periodic_y, Knowledge.domain_rect_periodic_z)

  if ((0 until numDims).map(cellWidth).distinct.length != 1)
    Logger.error("Trying to call IR_InitBlockForest with different step sizes.")

  override def generateFct() : IR_WaLBerlaPlainFunction = {
    // add deps
    IR_WaLBerlaCollection.get.addExternalDependency("blockforest/Initialization.h")

    var body : ListBuffer[IR_Statement] = ListBuffer()
    body += IR_Return(
      new IR_FunctionCall(IR_ExternalFunctionReference("blockforest::createUniformBlockGrid"),
        ListBuffer(
          wbBlocks(0), wbBlocks(1), wbBlocks(2),
          numCellsTotal(0), numCellsTotal(1), numCellsTotal(2),
          cellWidth(0),
          numProcesses(0), numProcesses(1), numProcesses(2),
          periodicity(0), periodicity(1), periodicity(2))))

    val func = IR_WaLBerlaPlainFunction(name, datatype, ListBuffer(), body)
    func.isInterfaceFunction = false
    func
  }

  override def prettyprint_decl() : String = prettyprint()
  override def name : String = "initBlockForest"
  override def name_=(newName : String) : Unit = name = newName
}
