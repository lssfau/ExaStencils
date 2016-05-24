package exastencils.polyhedron

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.TreeSet

import exastencils.datastructures.ir._
import isl.Conversions._

// since Scop can be cloned by Duplicate make sure NONE of the isl wrapper objects it uses is cloned by it
//   (register all required classes as not cloneable in IslUtil.scala)
class Scop(val root : LoopOverDimensions, var localContext : isl.Set, var globalContext : isl.Set, var optLevel : Int,
    var parallelize : Boolean, var origIterationCount : Array[Long]) {

  var nextMerge : Scop = null
  var remove : Boolean = false

  def getContext() : isl.Set = localContext.intersect(globalContext)
  var domain : isl.UnionSet = null
  var schedule : isl.UnionMap = null
  val stmts = new HashMap[String, (ListBuffer[Statement], ArrayBuffer[String])]()
  val decls = new ListBuffer[VariableDeclarationStatement]()

  final val loopVarTempl : String = "_i%d"
  val njuLoopVars = new ArrayBuffer[String]()
  val noParDims = new TreeSet[Int]()

  var reads, writes : isl.UnionMap = null
  var deadAfterScop : isl.UnionSet = null

  object deps {
    // dependences, which prevent parallelization AND vectorization
    var flowParVec : isl.UnionMap = null
    var antiOutParVec : isl.UnionMap = null
    // dependences, which prevent only parallelization, but NOT vectorization
    var flowPar : isl.UnionMap = null
    var antiOutPar : isl.UnionMap = null

    def flow : isl.UnionMap = {
      if (flowParVec == null)
        return flowPar
      else if (flowPar == null)
        return flowParVec
      else
        return Isl.simplify(flowParVec.union(flowPar))
    }

    private var inputCache : isl.UnionMap = null
    private var lazySetInput : () => isl.UnionMap = null
    private val lazyUpdateInputs = new ArrayBuffer[isl.UnionMap => isl.UnionMap]()

    def input_=(nju : isl.UnionMap) : Unit = {
      lazySetInput = null
      lazyUpdateInputs.clear()
      inputCache = nju
    }

    def setInputLazy(f : () => isl.UnionMap) : Unit = {
      lazySetInput = f
      lazyUpdateInputs.clear()
    }

    /** Maps a given Function1 to the input dependences, iff it was not null. */
    def mapInputLazy(f : isl.UnionMap => isl.UnionMap) : Unit = {
      lazyUpdateInputs += f
    }

    def input : isl.UnionMap = {
      if (lazySetInput != null) {
        inputCache = lazySetInput()
        lazySetInput = null
      }
      val it = lazyUpdateInputs.iterator
      while (it.hasNext && inputCache != null)
        inputCache = it.next()(inputCache)
      lazyUpdateInputs.clear()
      return inputCache
    }

    def validity() : isl.UnionMap = {
      return Isl.simplify(flowParVec.union(flowPar).union(antiOutParVec).union(antiOutPar))
    }

    def validityParVec() : isl.UnionMap = {
      return Isl.simplify(flowParVec.union(antiOutParVec))
    }

    def validityPar() : isl.UnionMap = {
      return Isl.simplify(flowPar.union(antiOutPar))
    }
  }

  def updateLoopVars() : Unit = {
    njuLoopVars.clear()
    var maxOut : Int = 0
    schedule.foreachMap({
      sched : isl.Map => maxOut = math.max(maxOut, sched.dim(isl.DimType.Out))
    })
    var i : Int = 0
    while (i < maxOut) {
      njuLoopVars += loopVarTempl.format(i)
      i += 1
    }
  }
}

object ScopNameMapping {

  private var count : Int = 0
  private final val id2expr = new HashMap[String, Expression]()
  private final val expr2id = new HashMap[Expression, String]()

  def id2expr(id : String) : Option[Expression] = {
    return id2expr.get(id)
  }

  def expr2id(expr : Expression) : String = {
    return expr2id.getOrElseUpdate(expr, {
      val id : String = expr match {
        case VariableAccess(str, _) if (str.length() < 5) =>
          str
        case _ =>
          count += 1
          "p" + count
      }
      id2expr.put(id, expr)
      id
    })
  }
}
