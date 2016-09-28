package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.iv
import exastencils.domain._
import exastencils.knowledge.{ Knowledge, _ }
import exastencils.prettyprinting.PpStream
import exastencils.util.AABB

case class IR_InitGeneratedDomain() extends IR_AbstractFunction with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl() = prettyprint
  override def name = "initDomain"

  // FIXME: introduce dedicated iv
  def mpiRank = if (Knowledge.mpi_enabled) IR_VariableAccess("mpiRank", IR_IntegerDatatype) else IR_IntegerConstant(0)

  def globalSize = IR_DomainCollection.getByIdentifier("global").get.asInstanceOf[RectangularDomain].shape.shapeData.asInstanceOf[AABB]
  def fragWidth(dim : Int) = globalSize.width(dim) / Knowledge.domain_rect_numFragsTotalAsVec(dim)

  def setupFragmentPosition() = {
    def localFragIndex(dim : Int) = (IR_LoopOverFragments.defIt / (0 until dim).map(Knowledge.domain_rect_numFragsPerBlockAsVec(_)).product) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(dim)
    def rankIndex(dim : Int) = (mpiRank / (0 until dim).map(dim => Knowledge.domain_rect_numBlocksAsVec(dim)).product) Mod Knowledge.domain_rect_numBlocksAsVec(dim)

    Knowledge.dimensions.map(dim =>
      IR_Assignment(iv.PrimitivePosition(dim),
        ((rankIndex(dim) * Knowledge.domain_rect_numFragsPerBlockAsVec(dim) + 0.5 + localFragIndex(dim)) * fragWidth(dim)) + globalSize.lower(dim))
    )
  }

  def setupFragmentPosBeginAndEnd() = {
    val begin = Knowledge.dimensions.map(dim =>
      IR_Assignment(iv.PrimitivePositionBegin(dim), iv.PrimitivePosition(dim) - 0.5 * fragWidth(dim)))
    val end = Knowledge.dimensions.map(dim =>
      IR_Assignment(iv.PrimitivePositionEnd(dim), iv.PrimitivePosition(dim) + 0.5 * fragWidth(dim)))
    begin ++ end
  }

  def setupFragmentId() = {
    IR_Assignment(iv.PrimitiveId(),
      Knowledge.dimensions.map(dim =>
        IR_ToInt((iv.PrimitivePosition(dim) - globalSize.lower(dim)) / fragWidth(dim))
          * (0 until dim).map(Knowledge.domain_rect_numFragsTotalAsVec(_)).product : IR_Expression).reduce(_ + _))
  }

  def setupFragmentIndex() = {
    Knowledge.dimensions.map(dim =>
      IR_Assignment(IR_IV_FragmentIndex(dim),
        IR_ToInt((iv.PrimitivePosition(dim) - globalSize.lower(dim)) / fragWidth(dim))))
  }

  def setupCommId() = {
    IR_Assignment(iv.CommId(),
      Knowledge.dimensions.map(dim =>
        (IR_ToInt((iv.PrimitivePosition(dim) - globalSize.lower(dim)) / fragWidth(dim))
          Mod Knowledge.domain_rect_numFragsPerBlockAsVec(dim)) * (0 until dim).map(Knowledge.domain_rect_numFragsPerBlockAsVec(_)).product : IR_Expression).reduce(_ + _))
  }

  override def expand() : Output[IR_Function] = {
    var body = ListBuffer[IR_Statement]()

    // TODO: move to main application
    if (Knowledge.mpi_enabled)
      body += IR_Assert(IR_EqEqExpression(s"mpiSize", Knowledge.domain_numBlocks),
        ListBuffer("\"Invalid number of MPI processes (\"", "mpiSize", "\") should be \"", Knowledge.mpi_numThreads),
        IR_FunctionCall("exit", 1))

    // compose fragment loop setting basic fragment information
    var fragStatements = ListBuffer[IR_Statement]()

    fragStatements ++= setupFragmentPosition()
    fragStatements ++= setupFragmentIndex()
    fragStatements += setupFragmentId()
    fragStatements += setupCommId()
    fragStatements ++= setupFragmentPosBeginAndEnd()

    // TODO: omp parallelization?
    body += IR_LoopOverFragments(fragStatements)

    body += ConnectFragments()

    // FIXME: move to app
    body += IR_FunctionCall("setupBuffers")

    IR_Function(IR_UnitDatatype, name, body)
  }
}