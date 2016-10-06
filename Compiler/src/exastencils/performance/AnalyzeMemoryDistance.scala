package exastencils.performance

import scala.collection.mutable.HashMap

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core.Duplicate
import exastencils.core.collectors.Collector
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir.IR_MultiDimFieldAccess
import exastencils.strategies.SimplifyStrategy

/**
  * = Analysis for memory access distance in iteration space in loop nests =
  *
  * @author Georg Altmann <georg.altmann@fau.de>
  *
  *         This is a naive attempt to extract the distances of the memory accesses in a loop nest.
  *         The intention is to provide a basis for the prediction of the performance of loop nests regarding CPU caches and
  *         for estimating optimal tile sizes for loop tiling.
  *
  *         The analysis assumes unit stride, hyperrectangle iteration spaces.
  *
  *         TODO:
  *         * collect accesses per field
  *         * print per field analysis
  */

/** Models the offset of an array access index (or field access in exastencils slang) in the iteration space.
  *
  * [[IndexOffset]] models the offset of a single subscript dimension, i.e. not the vector of offsets of an array access.
  *
  * Given a 3D field f:
  *
  * f[z+2,y-1,x]
  * Offsets: [2,-1,0]
  *
  * f[2,y-1,x]
  * Offsets: [0,-1,0]
  *
  * f[z,2*y,y]
  * Offsets: [0, inf, 0]
  * Here the use of y in the x dimension qualfies as a constant since y does not change in the inner loop.
  *
  *
  * An array subscript is treated as an affine linear expression in the iteration vector x.
  *
  * m * x + c
  *
  * For m =! 1 the offset from the iteration vector is non-constant and is treated as infinite.
  * The offset is then treated as infinite. The same applies to non-linear expressions in the iteration index vector.
  *
  */
sealed abstract class IndexOffset {

  def +(other : IndexOffset) : IndexOffset = {
    this match {
      case a : InfiniteIndexOffset => a
      case a : ConstantIndexOffset =>
        other match {
          case b : InfiniteIndexOffset => b
          case b : ConstantIndexOffset => ConstantIndexOffset(a.off + b.off)
        }
    }
  }

  def *(other : IndexOffset) : IndexOffset = {
    this match {
      case a : InfiniteIndexOffset => a
      case a : ConstantIndexOffset =>
        other match {
          case b : InfiniteIndexOffset => b
          case b : ConstantIndexOffset => ConstantIndexOffset(a.off * b.off)
        }
    }
  }
}

case class ConstantIndexOffset(off : Long) extends IndexOffset {
  override def toString() : String = off.toString
}

case class InfiniteIndexOffset() extends IndexOffset {
  override def toString() : String = "inf"
}

/** Analysis pass to extract memory access distances in iteration space. See [[IndexOffset]]
  */
object AnalyzeIterationDistance extends QuietDefaultStrategy(
  "Analyzing memory access distances in iteration space") {

  // TODO:
  //  * Constant only indices field[1,x,y] should map to [0,0,0] offsets and not to [1,0,0]
  //  * Use of loop variables of a higher dimension than the supscript dimension should map to zero offset,
  //  * example: [z,y,y] should map to [0,0,0] since y is a constant in the inner-loop

  var curLoop : IR_LoopOverDimensions = null
  var curFun : IR_Function = null

  val loopCollector = new Collector {
    override def leave(node : Node) : Unit = {
      node match {
        case fun : IR_Function => curFun = null
        case loop : IR_LoopOverDimensions => curLoop = null
          println("============================================")
          println("loop in function " + curFun.name)
          println("loop range %s -> %s".format(loop.indices.begin.prettyprint(), loop.indices.end.prettyprint()))
          println("loop stepSize "+ loop.stepSize.prettyprint())
        case _                            =>
      }
    }

    override def enter(node : Node) : Unit = {
      node match {
        case fun : IR_Function => curFun = fun
        case loop : IR_LoopOverDimensions => curLoop = loop
        case _                            =>
      }
    }

    override def reset() : Unit = curLoop = null
  }

  this.register(loopCollector)

  override def applyStandalone(node : Node) : Unit = {
    super.applyStandalone(node)
  }

  this += new Transformation("Offsets", {
    case fa : IR_MultiDimFieldAccess =>

      if (null != curLoop) {
        val field = fa.fieldSelection.field
        // Array of number of iterations per dimension
        val iterSpace = curLoop.maxIterationCount()
        val numDims = fa.fieldSelection.fieldLayout.numDimsGrid
        val fieldAccess = Duplicate(fa)
        SimplifyStrategy.doUntilDoneStandalone(fieldAccess)
        println(fa.fieldSelection.field.codeName + fieldAccess.index.prettyprint())
        val offsets = (0 to numDims - 1).map({ dim => // loop over dimensions
          val indexExpr = fieldAccess.index.indices(dim)
          AnalyzeSubscriptExpression(indexExpr, dim, iterSpace)
        }).toList
        println(s"subscript offsets: $offsets")
        // in multiples of datatype size
        val accessDist = accessDistance(offsets, iterSpace)
        println(s"access distance: $accessDist")

        val accessDistBytes =
          accessDist match {
            case co : ConstantIndexOffset  =>
              co * ConstantIndexOffset(fa.fieldSelection.field.gridDatatype.typicalByteSize)
            case inf : InfiniteIndexOffset => inf
          }
        println(s"access distance bytes: $accessDistBytes")

      }
      fa
  })

  /** Honer's-method-like scheme to compute distance from iteration vector to accessed element in iteration space.
    *
    * Assuming x is is the most continuous dimension of the array in memory.
    *
    * If all offsets are constant,
    *
    * dist = xoff + Nxiter * ( yoff + Nyiter * ( ... ( uoff ) ... )
    *
    * otherwise dist = inf
    *
    * @param offs      Constant offset in each dimension, most continuous first.
    * @param iterSpace Size of iteration space in each dimension, most continuous first.
    */
  def accessDistance(offs : Seq[IndexOffset], iterSpace : Seq[Long]) : IndexOffset = {

    assert(offs.size <= iterSpace.size)

    def rec(dim : Int = 0) : IndexOffset = {
      if (dim >= offs.size) {
        ConstantIndexOffset(0)
      } else {
        val off = offs(dim)
        off match {
          case infoff : InfiniteIndexOffset   => infoff
          case constoff : ConstantIndexOffset =>
            val highOff = rec(dim + 1)

            val iterDimSize = ConstantIndexOffset(iterSpace(dim))
            var higherdist = iterDimSize * highOff
            constoff + higherdist
        }
      }
    }

    rec(0)
  }
}

/** Analyze offset of array (field) subscript for single dimension.
  *
  * @param ssExpr         The subscript expression of a single dimension in the array (field) subscript.
  * @param dim            The dimension of the subscript expression corresponds to (0 is the the most continuous dimension of the array).
  * @param iterationSpace Size of each dimension in the iteration.
  */
class AnalyzeSubscriptExpression(val ssExpr : IR_Expression, val dim : Int, val iterationSpace : Array[Long]) {

  def apply() : IndexOffset = {
    ssExpr match {
      case IR_AdditionExpression(summands) => summands.map(s => addTerm(s, true)).reduce(_ + _)

      case IR_SubtractionExpression(left, right) => addTerm(left, true) + addTerm(right, false)

      case _ : IR_MultiplicationExpression => InfiniteIndexOffset()
      case _ : IR_DivisionExpression => InfiniteIndexOffset()
      case va : IR_VariableAccess               => addTerm(va, true)

    }
  }

  def negativeConstantIndexOffset(off : Long) = ConstantIndexOffset(-off)

  def signedConstantIndexOffset(off : Long, pos : Boolean) =
    if (pos) ConstantIndexOffset(off) else ConstantIndexOffset(-off)

  /** Evaluate addition term in subscript expression as an IndexOffset.
    *
    * @param sign true if expression has positive sign, false if expression has negative sign.
    */
  def addTerm(t : IR_Expression, sign : Boolean) : IndexOffset = {
    t match {
      case IR_VariableAccess(id, _)                              =>
        val loopVarDimOpt = loopIndexVarDim(id)
        loopVarDimOpt match {
          case Some(loopVarDim) =>
            if (loopVarDim == dim)
              ConstantIndexOffset(0)
            else
            // Loop index variable dimension does not match array index dimension. Assume this is infinitely far away.
              InfiniteIndexOffset()
          case None             =>
            // Non-loop index variable.
            InfiniteIndexOffset()
        }
      case IR_NegativeExpression(IR_VariableAccess(name, dType)) => addTerm(IR_VariableAccess(name, dType), !sign)
      case IR_IntegerConstant(x)                                 => signedConstantIndexOffset(x, sign)
      case IR_NegativeExpression(IR_IntegerConstant(x))          => signedConstantIndexOffset(x, !sign)

      // nested expressions -> not simplfied to (a * x + b) -> non-constant offset
      case ex : IR_AdditionExpression       => InfiniteIndexOffset()
      case ex : IR_MultiplicationExpression => InfiniteIndexOffset()
      case ex : IR_DivisionExpression     => InfiniteIndexOffset()
      case IR_ModuloExpression(left, right) =>
        right match {
          case IR_IntegerConstant(v) =>
            val c = math.signum(v)*(math.abs(v)-1)
            ConstantIndexOffset(c)
          case _ => InfiniteIndexOffset()
        }

      case _ => throw new Exception("Unhandled expression, " + t.getClass.getCanonicalName)
    }

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

object AnalyzeSubscriptExpression {
  def apply(idxExpr : IR_Expression, dim : Int, iterationSpace : Array[Long]) : IndexOffset = {
    new AnalyzeSubscriptExpression(idxExpr, dim, iterationSpace).apply()
  }
}
