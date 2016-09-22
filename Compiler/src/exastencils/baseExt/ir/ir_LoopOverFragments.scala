package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.mpi.MPI_Allreduce
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.prettyprinting.PpStream
import exastencils.strategies.ReplaceStringConstantsStrategy

object IR_LoopOverFragments {
  def apply(body : IR_Statement, reduction : Option[IR_Reduction]) = new IR_LoopOverFragments(ListBuffer(body), reduction)
  def apply(body : IR_Statement*) = new IR_LoopOverFragments(body.to[ListBuffer])

  // TODO: VariableAccess
  def defIt = "fragmentIdx"
}

case class IR_LoopOverFragments(var body : ListBuffer[IR_Statement], var reduction : Option[IR_Reduction] = None) extends IR_Statement with IR_Expandable {

  import IR_LoopOverFragments._

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def generateBasicLoop(parallelize : Boolean) = {
    val loop = if (parallelize)
      new IR_ForLoop(
        IR_VariableDeclaration(IR_IntegerDatatype, defIt, 0),
        IR_LowerExpression(defIt, Knowledge.domain_numFragmentsPerBlock),
        IR_PreIncrementExpression(defIt),
        body,
        reduction) with OMP_PotentiallyParallel
    else
      IR_ForLoop(
        IR_VariableDeclaration(IR_IntegerDatatype, defIt, 0),
        IR_LowerExpression(defIt, Knowledge.domain_numFragmentsPerBlock),
        IR_PreIncrementExpression(defIt),
        body,
        reduction)
    loop.annotate("numLoopIterations", Knowledge.domain_numFragmentsPerBlock)
    loop
  }

  override def expand() : Output[StatementList] = {
    var statements = new ListBuffer[IR_Statement]

    if (Knowledge.experimental_resolveUnreqFragmentLoops && Knowledge.domain_numFragmentsPerBlock <= 1) {
      // eliminate fragment loops in case of only one fragment per block
      statements = ListBuffer(IR_Scope(body))

      // replace references to old loop iterator
      ReplaceStringConstantsStrategy.toReplace = defIt
      ReplaceStringConstantsStrategy.replacement = IR_IntegerConstant(0)
      ReplaceStringConstantsStrategy.applyStandalone(statements)
    } else {
      val parallelize = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && this.isInstanceOf[OMP_PotentiallyParallel]
      val resolveOmpReduction = (
        parallelize
          && Platform.omp_version < 3.1
          && reduction.isDefined
          && ("min" == reduction.get.op || "max" == reduction.get.op))

      // basic loop

      if (!resolveOmpReduction) {
        statements += generateBasicLoop(parallelize)
      } else {
        // resolve max reductions
        val redOp = reduction.get.op
        val redExpName = reduction.get.target.name
        val redDatatype = None // FIXME: reduction.get.target.datatype
        def redExp = IR_VariableAccess(redExpName, redDatatype)
        val redExpLocalName = redExpName + "_red"
        def redExpLocal = IR_VariableAccess(redExpLocalName, redDatatype)

        // FIXME: this assumes real data types -> data type should be determined according to redExp
        val decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_RealDatatype, Knowledge.omp_numThreads), redExpLocalName, None)
        val init = (0 until Knowledge.omp_numThreads).map(fragIdx => IR_Assignment(IR_ArrayAccess(redExpLocal, fragIdx), redExp))
        val redOperands = ListBuffer[IR_Expression](redExp) ++ (0 until Knowledge.omp_numThreads).map(fragIdx => IR_ArrayAccess(redExpLocal, fragIdx) : IR_Expression)
        val red = IR_Assignment(redExp, if ("min" == redOp) IR_MinimumExpression(redOperands) else IR_MaximumExpression(redOperands))

        ReplaceStringConstantsStrategy.toReplace = redExp.prettyprint
        ReplaceStringConstantsStrategy.replacement = IR_ArrayAccess(redExpLocal, IR_VariableAccess("omp_tid", IR_IntegerDatatype))
        ReplaceStringConstantsStrategy.applyStandalone(body)
        body.prepend(IR_VariableDeclaration(IR_IntegerDatatype, "omp_tid", "omp_get_thread_num()"))

        statements += IR_Scope(ListBuffer[IR_Statement](decl)
          ++ init
          ++ ListBuffer[IR_Statement](generateBasicLoop(parallelize), red))
      }
    }

    if (Knowledge.mpi_enabled && reduction.isDefined) {
      statements += new MPI_Allreduce(IR_AddressofExpression(reduction.get.target), IR_RealDatatype, 1, reduction.get.op) // FIXME: get dt and cnt from reduction
    }

    statements
  }
}
