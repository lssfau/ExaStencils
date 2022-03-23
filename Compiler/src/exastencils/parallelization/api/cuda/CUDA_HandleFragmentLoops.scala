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
import exastencils.logger.Logger
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
    var fragLoop : IR_ScopedStatement with IR_HasParallelizationInfo
) extends IR_Statement with IR_Expandable {

  val iter = IR_LoopOverFragments.defIt
  def currCopy(copies : IR_VariableAccess) = IR_ArrayAccess(copies, iter)
  def reductionDt(redTarget : IR_Expression) = CUDA_Util.getReductionDatatype(redTarget)

  // replace occurrences with copy
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
    case _ : IR_ScalarDatatype =>
      IR_Assignment(redTarget, 0)
    case hodt : IR_HigherDimensionalDatatype =>
      matrixAssignment("std::fill", redTarget, 0.0, hodt.getSizeArray.product)
  }

  def copyReductionTarget(redTarget : IR_Expression, copies : IR_VariableAccess) = reductionDt(redTarget) match {
    case _ : IR_ScalarDatatype =>
      IR_Assignment(currCopy(copies), redTarget)
    case hodt : IR_HigherDimensionalDatatype =>
      matrixAssignment("std::copy", redTarget, currCopy(copies), hodt.getSizeArray.product)
  }

  def initCopies(copies : IR_VariableAccess, redTarget : IR_Expression) =  {
    val declCopies = IR_VariableDeclaration(copies)
    val initCopies = IR_LoopOverFragments(
      copyReductionTarget(redTarget, copies)).expandSpecial().inner
    val resetRedTarget = resetReductionTarget(redTarget) // reset initial value as it is already in the copies

    ListBuffer(
      declCopies,
      initCopies,
      resetRedTarget)
  }

  def finalizeReduction(op : String, redTarget : IR_Expression, copies : IR_VariableAccess) = {
    // finalize reduction
    val assign = reductionDt(redTarget) match {
      case mat : IR_MatrixDatatype =>

        // update reduction target
        val i = IR_VariableAccess("_i", IR_IntegerDatatype)
        val j = IR_VariableAccess("_j", IR_IntegerDatatype)
        val idx = i * mat.sizeN + j
        val dst = IR_ArrayAccess(redTarget, idx)
        val src = IR_ArrayAccess(currCopy(copies), idx)
        IR_ForLoop(IR_VariableDeclaration(i, IR_IntegerConstant(0)), IR_Lower(i, mat.sizeM), IR_PreIncrement(i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, mat.sizeN), IR_PreIncrement(j), ListBuffer[IR_Statement](
            IR_Assignment(dst, IR_BinaryOperators.createExpression(op, dst, src))))))

      case _ : IR_ScalarDatatype =>
        IR_Assignment(redTarget, IR_BinaryOperators.createExpression(op, redTarget, currCopy(copies)))
    }

    assign
  }

  def replaceAccesses(redTarget : IR_Expression, copies : IR_VariableAccess, body : ListBuffer[IR_Statement]) : Unit = {
    // replace redTarget accesses with accesses to frag copy
    CUDA_ReplaceReductionAccesses.redTarget = Duplicate(redTarget)
    CUDA_ReplaceReductionAccesses.replacement = Duplicate(currCopy(copies))
    CUDA_ReplaceReductionAccesses.applyStandalone(IR_Scope(body))
  }

  def addHandling(kernelFragLoop : IR_ScopedStatement with IR_HasParallelizationInfo) : ListBuffer[IR_Statement] = {
    var stmts = ListBuffer[IR_Statement]()
    // TODO: use IR_LoopOverFragments(CUDA_StreamSynchronize(CUDA_ComputeStream(iter))) later
    val synchroFragLoop = IR_LoopOverFragments()

    val loopTuple = kernelFragLoop match {
      case loop : IR_LoopOverFragments => Some((loop, loop.body))
      case loop @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == iter.name => Some((loop, loop.body))
      case _ => None
    }

    // early exit: no loop to be transformed
    if (loopTuple.isEmpty)
      return ListBuffer(kernelFragLoop)

    val loop = loopTuple.get._1
    val body = loopTuple.get._2

    // handle reductions
    if (loop.parallelization.reduction.isDefined) {
      val red = Duplicate(loop.parallelization.reduction.get)
      val redTarget = Duplicate(red.target)

      val counter = CUDA_HandleFragmentLoops.getReductionCounter(red.targetName)

      val copies = {
        val innerDt = reductionDt(redTarget) match {
          case scalar : IR_ScalarDatatype => scalar
          case hodt : IR_HigherDimensionalDatatype => IR_ArrayDatatype(hodt.resolveBaseDatatype, hodt.getSizeArray.product)
        }
        IR_VariableAccess(red.targetName + "_fragCpy" + counter, IR_ArrayDatatype(innerDt, Knowledge.domain_numFragmentsPerBlock))
      }

      stmts ++= initCopies(copies, redTarget) // init frag copies
      replaceAccesses(redTarget, copies, body) // replace accesses to frag copies
      synchroFragLoop.body += finalizeReduction(red.op, redTarget, copies) // accumulate frag copies at end
    }

    stmts += Duplicate(loop)
    stmts += synchroFragLoop // add stream synchro loop

    stmts
  }

  override def expand() : OutputType = {
    fragLoop match {
      case loop : IR_ScopedStatement with IR_HasParallelizationInfo => addHandling(loop)
      case _ => Logger.error("Invalid argument for \"fragLoop\" passed to CUDA_HandleFragmentLoopsWithReduction")
    }
  }
}
