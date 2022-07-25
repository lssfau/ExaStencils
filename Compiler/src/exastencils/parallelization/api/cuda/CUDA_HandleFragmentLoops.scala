package exastencils.parallelization.api.cuda

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.OutputType
import exastencils.parallelization.api.cuda.CUDA_HandleFragmentLoops.getReductionCounter
import exastencils.parallelization.ir.IR_HasParallelizationInfo

/// CUDA_HandleFragmentLoops
// - for multi-fragment kernel calls
// - uses fragment-local copies of the reduction variable's initial value
// - uses CUDA stream for each fragment
// - additional fragment loop added for stream synchronization and reduction result accumulation

object CUDA_HandleFragmentLoops {

  private var reductionCounters : mutable.HashMap[String, Int] = mutable.HashMap()

  def getReductionCounter(targetName : String) = {
    val c = reductionCounters.getOrElseUpdate(targetName, 0)
    reductionCounters(targetName) += 1
    c
  }
}

case class CUDA_HandleFragmentLoops(
    var fragLoop : IR_ScopedStatement with IR_HasParallelizationInfo,
    var streams : ListBuffer[CUDA_Stream]
) extends IR_Statement with IR_Expandable {

  val iter = IR_LoopOverFragments.defIt
  def currCopy(copies : IR_VariableAccess) = IR_ArrayAccess(copies, iter)
  def reductionDt(redTarget : IR_Expression) = CUDA_Util.getReductionDatatype(redTarget)

  // tmp buffer for reduction result (host)
  val reductionTmp = if (fragLoop.parallelization.reduction.isDefined) {
    val red = Duplicate(fragLoop.parallelization.reduction.get)
    val redTarget = Duplicate(red.target)

    val ret = reductionDt(redTarget) match {
      case mat : IR_MatrixDatatype    =>
        CUDA_ReductionResultBuffer(s"${ red.targetName }_${ getReductionCounter(red.targetName) }_reductionTmpMatrix", mat.resolveBaseDatatype, mat.sizeN * mat.sizeM)
      case scalar : IR_ScalarDatatype =>
        CUDA_ReductionResultBuffer(s"${ red.targetName }_${ getReductionCounter(red.targetName) }_reductionTmp", scalar.resolveBaseDatatype, 1)
    }

    Some(ret)
  } else {
    None
  }
  fragLoop.annotate(CUDA_Util.CUDA_REDUCTION_RESULT_BUF, reductionTmp)

  // replace occurrences of reduction target with its copy
  private object CUDA_ReplaceReductionAccesses extends QuietDefaultStrategy("Replace accesses to reduction targets") {
    var redTarget : IR_Expression = IR_NullExpression
    var replacement : IR_Expression = IR_NullExpression

    this += new Transformation("Replace", {
      case expr : IR_Expression if expr == redTarget => Duplicate(replacement)
    })
  }

  def matrixAssignment(stdFunc : String, dst : IR_Expression, src : IR_Expression, size : Int) =
    IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference(stdFunc, IR_UnitDatatype),
      ListBuffer[IR_Expression](Duplicate(dst), Duplicate(dst) + IR_IntegerConstant(size), src)))

  def resetReductionTarget(redTarget : IR_Expression) = reductionDt(redTarget) match {
    case _ : IR_ScalarDatatype               =>
      IR_Assignment(redTarget, 0)
    case hodt : IR_HigherDimensionalDatatype =>
      matrixAssignment("std::fill", redTarget, 0.0, hodt.getSizeArray.product)
  }

  def copyReductionTarget(redTarget : IR_Expression, op : String, copies : IR_VariableAccess) = {
    val tpe = redTarget.datatype.resolveBaseDatatype.prettyprint()
    val initVal : IR_Expression = op match {
      case "+" | "-" => 0
      case "*" => 1
      case "max" => IR_FunctionCall(s"std::numeric_limits<$tpe>::min")
      case "min" => IR_FunctionCall(s"std::numeric_limits<$tpe>::max")
    }

    reductionDt(redTarget) match {
      case _ : IR_ScalarDatatype               =>
        IR_Assignment(currCopy(copies), initVal)
      case hodt : IR_HigherDimensionalDatatype =>
        matrixAssignment("std::fill", currCopy(copies), initVal, hodt.getSizeArray.product)
    }
  }

  def initCopies(redTarget : IR_Expression, op : String, copies : IR_VariableAccess) = {
    val declCopies = IR_VariableDeclaration(copies)
    val initCopies = IR_LoopOverFragments(copyReductionTarget(redTarget, op, copies)).expandSpecial().inner

    ListBuffer(declCopies, initCopies)
  }

  // finalize reduction
  def finalizeReduction(op : String, redTarget : IR_Expression, reductionTmp : CUDA_ReductionResultBuffer, copies : IR_VariableAccess) : IR_Statement = {

    // update reduction target
    def getAssign(reductionResult : IR_Expression) = reductionDt(redTarget) match {
      case mat : IR_MatrixDatatype => // array returned

        val i = IR_VariableAccess("_i", IR_IntegerDatatype)
        val j = IR_VariableAccess("_j", IR_IntegerDatatype)
        val idx = i * mat.sizeN + j
        val dst = IR_ArrayAccess(redTarget, idx)
        val src = IR_ArrayAccess(reductionResult, idx)
        IR_ForLoop(IR_VariableDeclaration(i, IR_IntegerConstant(0)), IR_Lower(i, mat.sizeM), IR_PreIncrement(i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, mat.sizeN), IR_PreIncrement(j), ListBuffer[IR_Statement](
            IR_Assignment(dst, IR_BinaryOperators.createExpression(op, dst, src))))))

      case _ : IR_ScalarDatatype => // single value returned
        val dst = redTarget
        val src = if (reductionResult.datatype.isInstanceOf[IR_PointerDatatype])
          IR_ArrayAccess(reductionResult, 0)
        else
          reductionResult
        IR_Assignment(redTarget, IR_BinaryOperators.createExpression(op, dst, src))
    }

    // accumulate fragment copies into reduction variable
    if (Knowledge.cuda_preferredExecution == "Condition") {
      IR_IfCondition(Knowledge.cuda_executionCondition,
        getAssign(currCopy(copies)),
        getAssign(reductionTmp))
    } else {
      getAssign(reductionTmp)
    }
  }

  def replaceAccesses(redTarget : IR_Expression, copies : IR_VariableAccess, body : ListBuffer[IR_Statement]) : Unit = {
    // replace redTarget accesses with accesses to frag copy
    CUDA_ReplaceReductionAccesses.redTarget = Duplicate(redTarget)
    CUDA_ReplaceReductionAccesses.replacement = Duplicate(currCopy(copies))
    CUDA_ReplaceReductionAccesses.applyStandalone(IR_Scope(body))
  }

  override def expand() : OutputType = {
    var stmts = ListBuffer[IR_Statement]()

    // sync before/after kernel calls in separate frag loop
    val syncBeforeFragLoop = IR_LoopOverFragments(streams.flatMap(stream => CUDA_Stream.genSynchronize(stream, before = true)))
    val syncAfterFragLoop = IR_LoopOverFragments(streams.flatMap(stream => CUDA_Stream.genSynchronize(stream, before = false)))

    val loopTuple = fragLoop match {
      case loop : IR_LoopOverFragments                                                               => Some((loop, loop.body))
      case loop @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == iter.name => Some((loop, loop.body))
      case _                                                                                         => None
    }

    // early exit: no loop to be transformed
    if (loopTuple.isEmpty)
      return ListBuffer(fragLoop)

    // fetch (resolved) frag loop (=> scoped stmt with ParallelizationInfo) and its body
    val loop = loopTuple.get._1
    val body = loopTuple.get._2

    // handle reductions
    if (loop.parallelization.reduction.isDefined) {
      val red = Duplicate(loop.parallelization.reduction.get)
      val redTarget = Duplicate(red.target)

      // move reduction towards "synchroFragLoop"
      // -> OpenMP/MPI reduction occurs after accumulation in "synchroFragLoop"
      loop.parallelization.reduction = None
      syncAfterFragLoop.parallelization.reduction = Some(red)

      val counter = CUDA_HandleFragmentLoops.getReductionCounter(red.targetName)
      val copies = {
        val innerDt = reductionDt(redTarget) match {
          case scalar : IR_ScalarDatatype          => scalar
          case hodt : IR_HigherDimensionalDatatype => IR_ArrayDatatype(hodt.resolveBaseDatatype, hodt.getSizeArray.product)
        }
        IR_VariableAccess(red.targetName + "_fragCpy" + counter, IR_ArrayDatatype(innerDt, Knowledge.domain_numFragmentsPerBlock))
      }

      stmts ++= initCopies(redTarget, red.op, copies) // init frag copies
      replaceAccesses(redTarget, copies, body) // replace accesses to frag copies
      syncAfterFragLoop.body += finalizeReduction(red.op, redTarget, reductionTmp.get, copies) // accumulate frag copies at end
    }

    stmts += syncBeforeFragLoop // add stream synchro loop before kernel calls
    stmts += Duplicate(loop)
    stmts += syncAfterFragLoop // add stream synchro loop after kernel calls

    stmts
  }
}
