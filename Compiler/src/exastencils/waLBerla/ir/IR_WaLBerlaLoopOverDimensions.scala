package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Cast
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Lower
import exastencils.base.ir.IR_ScopedStatement
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_SpecialExpandable
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_FieldIteratorAccess
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.parallelization.ir.IR_HasParallelizationInfo
import exastencils.parallelization.ir.IR_ParallelizationInfo

case class IR_WaLBerlaLoopOverDimensions(
    var numDimensions : Int,
    var indices : IR_ExpressionIndexRange,
    var body : ListBuffer[IR_Statement],
    var stepSize : IR_ExpressionIndex = null, // actual default set in constructor
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo(),
    var condition : Option[IR_Expression] = None,
    var genOMPThreadLoop : Boolean = false
) extends IR_ScopedStatement with IR_SpecialExpandable with IR_HasParallelizationInfo {

  val loopOverDims = IR_LoopOverDimensions(numDimensions, indices, body)

  if (stepSize == null) {
    stepSize = loopOverDims.stepSize
  } else {
    indices.end = loopOverDims.indices.end
  }

  // TODO optimizations as in IR_LoopOverDimensions
  def expandSpecial() : ListBuffer[IR_Statement] = {
    var wrappedBody : ListBuffer[IR_Statement] = body

    // add internal condition (e.g. RB)
    if (condition.isDefined)
      wrappedBody = ListBuffer[IR_Statement](IR_IfCondition(condition.get, wrappedBody))

    for (d <- 0 until numDimensions) {
      def it = IR_FieldIteratorAccess(d)
      val decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FieldIteratorAccess(d).name, indices.begin(d))

      val cond = IR_Lower(it, IR_Cast(it.datatype, indices.end(d)))
      val incr = IR_Assignment(it, stepSize(d), "+=")
      val loop = new IR_ForLoop(decl, cond, incr, wrappedBody, Duplicate(parallelization))

      wrappedBody = ListBuffer[IR_Statement](loop)
    }

    wrappedBody
  }
}

/// IR_WaLBerlaResolveLoopOverDimensions

object IR_WaLBerlaResolveLoopOverDimensions extends DefaultStrategy("Resolve LoopOverDimensions nodes") {
  this += new Transformation("Resolve", {
    case loop : IR_WaLBerlaLoopOverDimensions => loop.expandSpecial()
  })
}
