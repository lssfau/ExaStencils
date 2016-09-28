package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.iv
import exastencils.domain._
import exastencils.knowledge._
import exastencils.prettyprinting.PpStream
import exastencils.util.AABB

case class IR_InitGeneratedDomain() extends IR_AbstractFunction with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl = prettyprint
  override def name = "initDomain"

  def setupFragmentId(globalSize : AABB) = {
    def fragWidth(dim : Int) = globalSize.width(dim) / Knowledge.domain_rect_numFragsTotalAsVec(dim)

    IR_Assignment(iv.PrimitiveId(),
      Knowledge.dimensions.map(dim =>
        IR_ToInt((iv.PrimitivePosition(dim) - globalSize.lower(dim)) / fragWidth(dim))
          * (0 until dim).map(Knowledge.domain_rect_numFragsTotalAsVec(_)).product : IR_Expression).reduce(_ + _))
  }

  def setupCommId(globalSize : AABB) = {
    def fragWidth(dim : Int) = globalSize.width(dim) / Knowledge.domain_rect_numFragsTotalAsVec(dim)

    IR_Assignment(iv.CommId(),
      Knowledge.dimensions.map(dim =>
        (IR_ToInt((iv.PrimitivePosition(dim) - globalSize.lower(dim)) / fragWidth(dim))
          Mod Knowledge.domain_rect_numFragsPerBlockAsVec(dim)) * (0 until dim).map(Knowledge.domain_rect_numFragsPerBlockAsVec(_)).product : IR_Expression).reduce(_ + _))
  }

  override def expand : Output[IR_Function] = {
    val globalDomain = IR_DomainCollection.getByIdentifier("global").get
    val globalSize = globalDomain.asInstanceOf[RectangularDomain].shape.shapeData.asInstanceOf[AABB]

    def fragWidth(dim : Int) = globalSize.width(dim) / Knowledge.domain_rect_numFragsTotalAsVec(dim)

    // FIXME: introduce dedicated iv
    def mpiRank = if (Knowledge.mpi_enabled) IR_VariableAccess("mpiRank", IR_IntegerDatatype) else IR_IntegerConstant(0)

    var body = ListBuffer[IR_Statement]()

    // TODO: move to main application
    if (Knowledge.mpi_enabled)
      body += IR_Assert(IR_EqEqExpression(s"mpiSize", Knowledge.domain_numBlocks),
        ListBuffer("\"Invalid number of MPI processes (\"", "mpiSize", "\") should be \"", Knowledge.mpi_numThreads),
        IR_FunctionCall("exit", 1))

    def rankPos(dim : Int) = (mpiRank / (0 until dim).map(dim => Knowledge.domain_rect_numBlocksAsVec(dim)).product) Mod Knowledge.domain_rect_numBlocksAsVec(dim)

    // compose fragment loop setting basic fragment information
    var fragStatements = ListBuffer[IR_Statement]()

    // FIXME: remove vec3 usage -> first step: convert usage to element-wise usage
    def x = IR_LoopOverFragments.defIt Mod Knowledge.domain_rect_numFragsPerBlockAsVec(0)
    def y = (IR_LoopOverFragments.defIt / Knowledge.domain_rect_numFragsPerBlockAsVec(0)) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(1)
    def z = (IR_LoopOverFragments.defIt / (Knowledge.domain_rect_numFragsPerBlockAsVec(0) * Knowledge.domain_rect_numFragsPerBlockAsVec(1))) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(2)

    fragStatements += IR_Assignment(iv.PrimitivePosition(), IR_FunctionCall("Vec3", ListBuffer[IR_Expression](
      ((rankPos(0) * Knowledge.domain_rect_numFragsPerBlockAsVec(0) + 0.5 + x) * fragWidth(0)) + globalSize.lower(0),
      if (Knowledge.dimensionality > 1) ((rankPos(1) * Knowledge.domain_rect_numFragsPerBlockAsVec(1) + 0.5 + y) * fragWidth(1)) + globalSize.lower(1) else 0,
      if (Knowledge.dimensionality > 2) ((rankPos(2) * Knowledge.domain_rect_numFragsPerBlockAsVec(2) + 0.5 + z) * fragWidth(2)) + globalSize.lower(2) else 0)))

    fragStatements += IR_Assignment(iv.PrimitiveIndex(), PointToFragmentIndex(iv.PrimitivePosition()))

    fragStatements += setupFragmentId(globalSize)
    fragStatements += setupCommId(globalSize)

    for (dim <- Knowledge.dimensions) {
      fragStatements += IR_Assignment(iv.PrimitivePositionBegin(dim), iv.PrimitivePosition(dim) - 0.5 * fragWidth(dim))
      fragStatements += IR_Assignment(iv.PrimitivePositionEnd(dim), iv.PrimitivePosition(dim) + 0.5 * fragWidth(dim))
    }

    // TODO: omp parallelization?
    body += IR_LoopOverFragments(fragStatements)

    body += ConnectFragments()

    // FIXME: move to app
    body += IR_FunctionCall("setupBuffers")

    IR_Function(IR_UnitDatatype, name, body)
  }
}