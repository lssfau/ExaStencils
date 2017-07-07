package exastencils.polyhedron

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import isl.Conversions._

// since Scop can be cloned by Duplicate make sure NONE of the isl wrapper objects it uses is cloned by it
//   (register all required classes as not cloneable in IslUtil.scala)
class Scop(val root : IR_LoopOverDimensions, var localContext : isl.Set, var globalContext : isl.Set, var optLevel : Int,
    var parallelize : Boolean, var origIterationCount : Array[Long]) {

  var nextMerge : Scop = null
  var remove : Boolean = false

  def getContext() : isl.Set = localContext.intersect(globalContext)
  var domain : isl.UnionSet = null
  var schedule : isl.UnionMap = null
  val stmts = new HashMap[String, (ListBuffer[IR_Statement], ArrayBuffer[String])]()
  val decls = new ListBuffer[IR_VariableDeclaration]()

  final val loopVarTempl : String = "_i%d"
  val njuLoopVars = new ArrayBuffer[String]()
  val noParDims = new TreeSet[Int]()

  var reads, writes : isl.UnionMap = null
  var deadAfterScop : isl.UnionSet = null

  def tileSizes : Array[Int] = root.tileSize

  object deps {
    // dependences, which prevent parallelization AND vectorization
    var flowParVec : isl.UnionMap = null
    var antiOutParVec : isl.UnionMap = null
    // dependences, which prevent only parallelization, but NOT vectorization
    var flowPar : isl.UnionMap = null
    var antiOutPar : isl.UnionMap = null

    def flow : isl.UnionMap = {
      if (flowParVec == null)
        flowPar
      else if (flowPar == null)
        flowParVec
      else
        Isl.simplify(flowParVec.union(flowPar))
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
      inputCache
    }

    def validity() : isl.UnionMap = {
      Isl.simplify(flowParVec.union(flowPar).union(antiOutParVec).union(antiOutPar))
    }

    def validityParVec() : isl.UnionMap = {
      Isl.simplify(flowParVec.union(antiOutParVec))
    }

    def validityPar() : isl.UnionMap = {
      Isl.simplify(flowPar.union(antiOutPar))
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
  private final val id2expr = new HashMap[String, IR_Expression]()
  private final val expr2id = new HashMap[IR_Expression, String]()

  def id2expr(id : String) : Option[IR_Expression] = {
    id2expr.get(id)
  }

  def expr2id(expr : IR_Expression, alias : IR_Expression = null) : String = {
    expr2id.getOrElseUpdate(expr, {
      val id : String =
        if (alias != null && expr2id.contains(alias))
          expr2id(alias)
        else expr match {
          case IR_VariableAccess(str, _) if str.length() < 5 =>
            str
          case _                                             =>
            count += 1
            "p" + count
        }
      id2expr.put(id, expr)
      if (alias != null)
        expr2id.put(alias, id)
      id
    })
  }
}
