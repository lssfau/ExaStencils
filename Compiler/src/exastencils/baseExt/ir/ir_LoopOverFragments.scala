package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.{ ArrayAccess, Expandable, Reduction, VariableDeclarationStatement, _ }
import exastencils.knowledge._
import exastencils.mpi.MPI_Allreduce
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.prettyprinting.PpStream
import exastencils.strategies.ReplaceStringConstantsStrategy

object IR_LoopOverFragments {
  def apply(body : IR_Statement, reduction : Option[Reduction]) = new IR_LoopOverFragments(ListBuffer(body), reduction)
  def apply(body : IR_Statement*) = new IR_LoopOverFragments(body.to[ListBuffer])

  def defIt = "fragmentIdx"
}

case class IR_LoopOverFragments(var body : ListBuffer[IR_Statement], var reduction : Option[Reduction] = None) extends IR_Statement with Expandable {

  import IR_LoopOverFragments._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverFragments\n"

  def generateBasicLoop(parallelize : Boolean) = {
    val loop = if (parallelize)
      new IR_ForLoop(
        VariableDeclarationStatement(IR_IntegerDatatype, defIt, Some(0)),
        IR_LowerExpression(defIt, Knowledge.domain_numFragmentsPerBlock),
        IR_PreIncrementExpression(defIt),
        body,
        reduction) with OMP_PotentiallyParallel
    else
      IR_ForLoop(
        VariableDeclarationStatement(IR_IntegerDatatype, defIt, Some(0)),
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
        def redExp = IR_VariableAccess(redExpName, None)
        val redExpLocalName = redExpName + "_red"
        def redExpLocal = IR_VariableAccess(redExpLocalName, None)

        // FIXME: this assumes real data types -> data type should be determined according to redExp
        val decl = VariableDeclarationStatement(IR_ArrayDatatype(IR_RealDatatype, Knowledge.omp_numThreads), redExpLocalName, None)
        val init = (0 until Knowledge.omp_numThreads).map(fragIdx => IR_Assignment(ArrayAccess(redExpLocal, fragIdx), redExp))
        val redOperands = ListBuffer[IR_Expression](redExp) ++ (0 until Knowledge.omp_numThreads).map(fragIdx => ArrayAccess(redExpLocal, fragIdx) : IR_Expression)
        val red = IR_Assignment(redExp, if ("min" == redOp) IR_MinimumExpression(redOperands) else IR_MaximumExpression(redOperands))

        ReplaceStringConstantsStrategy.toReplace = redExp.prettyprint
        ReplaceStringConstantsStrategy.replacement = ArrayAccess(redExpLocal, IR_VariableAccess("omp_tid", Some(IR_IntegerDatatype)))
        ReplaceStringConstantsStrategy.applyStandalone(body)
        body.prepend(VariableDeclarationStatement(IR_IntegerDatatype, "omp_tid", Some("omp_get_thread_num()")))

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
