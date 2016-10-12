package exastencils.performance

import scala.collection.immutable.HashMap

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.core.Duplicate
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.field.ir.IR_MultiDimFieldAccess
import exastencils.optimization.ir._

object KernelSubscriptAnalysis extends QuietDefaultStrategy(
  "Analyzing array subscripts of loop kernels") {

  var curLoop : IR_LoopOverDimensions = null
  var curFun : IR_Function = null

  val loopCollector = new Collector {
    override def leave(node : Node) : Unit = {
      node match {
        case fun : IR_Function            => curFun = null
        case loop : IR_LoopOverDimensions => curLoop = null
          println("============================================")
          println("loop in function " + curFun.name)
          println("loop range %s -> %s".format(loop.indices.begin.prettyprint(), loop.indices.end.prettyprint()))
          println("loop stepSize " + loop.stepSize.prettyprint())
          println("loop indices " + loop.indices.print)
        case _                            =>
      }
    }

    override def enter(node : Node) : Unit = {
      node match {
        case fun : IR_Function            => curFun = fun
        case loop : IR_LoopOverDimensions => curLoop = loop
        case _                            =>
      }
    }

    override def reset() : Unit = curLoop = null
  }

  this.register(loopCollector)

  /** Symbolically increase loop variables to next iteration and compute step size in all dimensions.
    *
    * This is just for experimentation. No useful transformation here.
    */
  def debugFieldAccessStepSize(fa : IR_MultiDimFieldAccess, curLoop : IR_LoopOverDimensions) : Unit = {
    val origIndex = Duplicate(fa.index)
    val steppedIndex = Duplicate(fa.index)
    val stepFieldAccesses = new StepFieldAccesses(curLoop)
    stepFieldAccesses.applyStandalone(steppedIndex)

    println("orig index: " + origIndex.prettyprint())
    println("stepped index: " + steppedIndex.prettyprint())

    val diffIndex = steppedIndex - origIndex;
    //    IR_ExpressionIndex(diffIndex:_*)
    println("diff : " + diffIndex.prettyprint())

    val diffIndexSimpl = IR_ExpressionIndex(diffIndex.map(x => IR_SimplifyExpression.simplifyIntegralExpr(x)).toSeq : _*)
//    SimplifyExpression.simplifyIntegralExpr(diffIndex)
    IR_GeneralSimplify.doUntilDoneStandalone(diffIndex)
//    diffIndex.foreach(x => SimplifyStrategy.doUntilDoneStandalone(x))
    println("diff simplified: " + diffIndexSimpl.prettyprint())

  }

  this += new Transformation("collect IR_MultiDimFieldAccess`es in IR_LoopOverDimensions", {
    case fa : IR_MultiDimFieldAccess =>
      if (null != curLoop) {
        debugFieldAccessStepSize(fa, curLoop)
      }
      fa
  })
}

/** Transforms an IR_ExpressionIndex by symbolically stepping it to the next iteration with the step size of the loop in
  * each dimension.
  *
  * I.e. [y][x] -> [y+1][x+1]
  *
  * Apply to cloned IR_ExpressionIndex of IR_MultiDimFieldAccess. */
class StepFieldAccesses(val loop : IR_LoopOverDimensions) extends QuietDefaultStrategy(
  "Stepping loop indices in field accesses") {
  val trafo : PartialFunction[Node, Transformation.OutputType] = {

    case va : IR_VariableAccess =>

      val dimOpt = loopIndexVarDim(va.name)
      // assuming va is not a index variable access if loopIndexVarDim does not return Some[Int]

      if (dimOpt.isDefined) {
        val dim = dimOpt.get
        val stepSize = loop.stepSize(dim)
        //        println("XX stepSize: " + stepSize.prettyprint())
        val steppedIndex = IR_AdditionExpression(va, stepSize)
//        println("XX steppedIndex: " + steppedIndex.prettyprint())
        steppedIndex
      } else {
        va
      }
    //    case x:IR_Node =>
    //      val pretty = x match {
    //        case y:PrettyPrintable => " {%s}".format(y.prettyprint())
    //        case _ => ""
    //      }
    //      println("found %s%s".format(x.getClass().getCanonicalName(), pretty))
    //      x
  }
  this += new Transformation("Trafo", trafo, false)

  override def applyStandalone(node : Node) : Unit = {
    node match {
      case x : IR_ExpressionIndex => println("IR_ExpressionIndex: %s".format(x.prettyprint()))
      case _                      =>
    }

    super.applyStandalone(node)
  }

  def isloopIndexVar(id : String) : Boolean = {
    Seq("x", "y", "z", "u", "v", "w").contains(id)
  }

  def loopIndexVarDim(id : String) : Option[Int] = {
    val map = HashMap(
      "x" -> 0,
      "y" -> 1,
      "z" -> 2,
      "w" -> 3,
      "v" -> 4,
      "u" -> 5)
    map.get(id)
  }
}

