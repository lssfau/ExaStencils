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

/// CUDA_HandleFragmentLoopsWithReduction
// - for multi-fragment reductions
// - uses fragment-local copies of the reduction variable's initial value
// - otherwise an updated value, which was previously computed from another fragment,
//   may cause an over-accumulation when performing the reduction in the kernel with the updated value

object CUDA_HandleFragmentLoopsWithReduction {

  private var reductionCounters : mutable.HashMap[String, Int] = mutable.HashMap()

  def getReductionCounter(targetName : String) = {
    val c = reductionCounters.getOrElseUpdate(targetName, 0)
    reductionCounters(targetName) += 1
    c
  }
}

case class CUDA_HandleFragmentLoopsWithReduction(
    var fragLoop : IR_ScopedStatement with IR_HasParallelizationInfo,
    var reduction: IR_Reduction
) extends IR_Statement with IR_Expandable {

  val iter = IR_LoopOverFragments.defIt

  val redTarget = reduction.target
  val reductionDt = CUDA_Util.getReductionDatatype(redTarget)

  val counter = CUDA_HandleFragmentLoopsWithReduction.getReductionCounter(reduction.targetName)

  val copies = {
    val innerDt = reductionDt match {
      case scalar : IR_ScalarDatatype => scalar
      case hodt : IR_HigherDimensionalDatatype => IR_ArrayDatatype(hodt.resolveBaseDatatype, hodt.getSizeArray.product)
    }
    IR_VariableAccess(reduction.targetName + "_" + counter, IR_ArrayDatatype(innerDt, Knowledge.domain_numFragmentsPerBlock))
  }
  val currCopy = IR_ArrayAccess(copies, iter)

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

  def resetReductionTarget() = reductionDt match {
    case _ : IR_ScalarDatatype =>
      IR_Assignment(redTarget, 0)
    case hodt : IR_HigherDimensionalDatatype =>
      matrixAssignment("std::fill", redTarget, 0.0, hodt.getSizeArray.product)
  }

  def copyReductionTarget() = reductionDt match {
    case _ : IR_ScalarDatatype =>
      IR_Assignment(currCopy, redTarget)
    case hodt : IR_HigherDimensionalDatatype =>
      matrixAssignment("std::copy", redTarget, currCopy, hodt.getSizeArray.product)
  }

  def initCopies() = ListBuffer(
    IR_VariableDeclaration(copies), // declare copies
    IR_LoopOverFragments( // init copies
      copyReductionTarget()),
    resetReductionTarget()) // reset initial value as it is already in the copies

  def finalizeReduction(body : ListBuffer[IR_Statement]) = {
    // finalize reduction
    val assign = reductionDt match {
      case mat : IR_MatrixDatatype =>

        // update reduction target
        val i = IR_VariableAccess("_i", IR_IntegerDatatype)
        val j = IR_VariableAccess("_j", IR_IntegerDatatype)
        val idx = i * mat.sizeN + j
        val dst = IR_ArrayAccess(redTarget, idx)
        val src = IR_ArrayAccess(currCopy, idx)
        IR_ForLoop(IR_VariableDeclaration(i, IR_IntegerConstant(0)), IR_Lower(i, mat.sizeM), IR_PreIncrement(i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, mat.sizeN), IR_PreIncrement(j), ListBuffer[IR_Statement](
            IR_Assignment(dst, IR_BinaryOperators.createExpression(reduction.op, dst, src))))))

      case _ : IR_ScalarDatatype =>
        IR_Assignment(redTarget, IR_BinaryOperators.createExpression(reduction.op, redTarget, currCopy))
    }

    body :+ assign
  }

  def replaceAccesses(body : ListBuffer[IR_Statement]) = {
    // replace occurrences
    CUDA_ReplaceReductionAccesses.redTarget = redTarget
    CUDA_ReplaceReductionAccesses.replacement = currCopy
    CUDA_ReplaceReductionAccesses.applyStandalone(IR_Scope(body))
  }

  def addHandling(loop : IR_ForLoop) = {
    replaceAccesses(loop.body)
    loop.body = finalizeReduction(loop.body)
    initCopies() :+ loop
  }

  def addHandling(loop : IR_LoopOverFragments) = {
    replaceAccesses(loop.body)
    loop.body = finalizeReduction(loop.body)
    initCopies() :+ loop
  }

  override def expand() : OutputType = {
    fragLoop match {
      case loop : IR_LoopOverFragments => addHandling(loop)
      case loop @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == iter.name => addHandling(loop)
      case _ => Logger.error("Invalid argument for \"fragLoop\" passed to CUDA_HandleFragmentLoopsWithReduction")
    }
  }
}
