package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.iv
import exastencils.globals.ir.IR_AllocateDataFunction
import exastencils.knowledge._
import exastencils.mpi.ir.MPI_IV_MpiRank
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.prettyprinting.PpStream
import exastencils.util.AABB

case class IR_InitGeneratedDomain() extends IR_AbstractFunction with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl() = prettyprint
  override def name = "initDomain"

  def globalSize = IR_DomainCollection.getByIdentifier("global").get.asInstanceOf[RectangularDomain].shape.shapeData.asInstanceOf[AABB]
  def fragWidth(dim : Int) = globalSize.width(dim) / Knowledge.domain_rect_numFragsTotalAsVec(dim)

  def setupFragmentPosition() = {
    def localFragIndex(dim : Int) = (IR_LoopOverFragments.defIt / (0 until dim).map(Knowledge.domain_rect_numFragsPerBlockAsVec(_)).product) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(dim)
    def rankIndex(dim : Int) = (MPI_IV_MpiRank / (0 until dim).map(dim => Knowledge.domain_rect_numBlocksAsVec(dim)).product) Mod Knowledge.domain_rect_numBlocksAsVec(dim)

    Knowledge.dimensions.map(dim =>
      IR_Assignment(IR_IV_FragmentPosition(dim),
        ((rankIndex(dim) * Knowledge.domain_rect_numFragsPerBlockAsVec(dim) + 0.5 + localFragIndex(dim)) * fragWidth(dim)) + globalSize.lower(dim))
    )
  }

  def setupFragmentPosBeginAndEnd() = {
    val begin = Knowledge.dimensions.map(dim =>
      IR_Assignment(IR_IV_FragmentPositionBegin(dim), IR_IV_FragmentPosition(dim) - 0.5 * fragWidth(dim)))
    val end = Knowledge.dimensions.map(dim =>
      IR_Assignment(IR_IV_FragmentPositionEnd(dim), IR_IV_FragmentPosition(dim) + 0.5 * fragWidth(dim)))
    begin ++ end
  }

  def setupFragmentId() = {
    IR_Assignment(IR_IV_FragmentId(),
      Knowledge.dimensions.map(dim =>
        IR_ToInt((IR_IV_FragmentPosition(dim) - globalSize.lower(dim)) / fragWidth(dim))
          * (0 until dim).map(Knowledge.domain_rect_numFragsTotalAsVec(_)).product : IR_Expression).reduce(_ + _))
  }

  def setupFragmentIndex() = {
    Knowledge.dimensions.map(dim =>
      IR_Assignment(IR_IV_FragmentIndex(dim),
        IR_ToInt((IR_IV_FragmentPosition(dim) - globalSize.lower(dim)) / fragWidth(dim))))
  }

  def setupCommId() = {
    IR_Assignment(iv.CommId(),
      Knowledge.dimensions.map(dim =>
        (IR_ToInt((IR_IV_FragmentPosition(dim) - globalSize.lower(dim)) / fragWidth(dim))
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

    val fragLoop = new IR_LoopOverFragments(fragStatements) with OMP_PotentiallyParallel
    fragLoop.parallelization.potentiallyParallel = true
    body += fragLoop

    body += IR_ConnectFragments()

    // FIXME: move to app
    body += IR_FunctionCall(IR_AllocateDataFunction.fctName)

    IR_Function(IR_UnitDatatype, name, body)
  }
}
