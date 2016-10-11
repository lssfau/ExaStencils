package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.mpi.ir.MPI_AllReduce
import exastencils.parallelization.ir._
import exastencils.prettyprinting.PpStream
import exastencils.strategies.ReplaceStringConstantsStrategy

object IR_LoopOverFragments {
  def apply(body : IR_Statement*) = new IR_LoopOverFragments(body.to[ListBuffer])
  def apply(body : IR_Statement, parallelization : IR_ParallelizationInfo) = new IR_LoopOverFragments(ListBuffer(body), parallelization)

  // TODO: VariableAccess
  def defIt = "fragmentIdx"
}

case class IR_LoopOverFragments(
    var body : ListBuffer[IR_Statement],
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo()) extends IR_Statement with IR_Expandable with IR_HasParallelizationInfo {

  import IR_LoopOverFragments._

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def generateBasicLoop() = {
    val loop = IR_ForLoop(
      IR_VariableDeclaration(IR_IntegerDatatype, defIt, 0),
      IR_LowerExpression(defIt, Knowledge.domain_numFragmentsPerBlock),
      IR_PreIncrementExpression(defIt),
      body,
      parallelization)
    loop.annotate("numLoopIterations", Knowledge.domain_numFragmentsPerBlock)
    loop
  }

  override def expand() : Output[StatementList] = {
    var statements = new ListBuffer[IR_Statement]

    // TODO: move reduction resolution to separate strategy - when to insert this? multiple times?

    if (Knowledge.experimental_resolveUnreqFragmentLoops && Knowledge.domain_numFragmentsPerBlock <= 1) {
      // eliminate fragment loops in case of only one fragment per block
      statements = ListBuffer(IR_Scope(body))

      // replace references to old loop iterator
      ReplaceStringConstantsStrategy.toReplace = defIt
      ReplaceStringConstantsStrategy.replacement = IR_IntegerConstant(0)
      ReplaceStringConstantsStrategy.applyStandalone(statements)
    } else {
      // TODO: extract
      parallelization.potentiallyParallel = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && parallelization.potentiallyParallel

      // basic loop
      statements += generateBasicLoop()
    }

    if (Knowledge.mpi_enabled && parallelization.reduction.isDefined) {
      // FIXME: get dt and cnt from reduction
      statements += MPI_AllReduce(IR_AddressofExpression(parallelization.reduction.get.target), IR_RealDatatype, 1, parallelization.reduction.get.op)
    }

    statements
  }
}
