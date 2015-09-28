package exastencils.polyhedron

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.TreeSet

import exastencils.datastructures.Node
import exastencils.datastructures.ir._
import isl.Conversions._

// since Scop can be cloned by Duplicate make sure NONE of the isl wrapper objects it uses is cloned by it
//   (register all required classes as not cloneable in IslUtil.scala)
class Scop(val root : LoopOverDimensions, var optLevel : Int, var parallelize : Boolean, var origIterationCount : Array[Long]) {

  var nextMerge : Scop = null
  var remove : Boolean = false

  var domain : isl.UnionSet = null
  var schedule : isl.UnionMap = null
  val stmts = new HashMap[String, (ListBuffer[Statement], ArrayBuffer[String])]()
  val decls = new ListBuffer[VariableDeclarationStatement]()

  val njuLoopVars = new ArrayBuffer[String]()
  val noParDims = new TreeSet[Int]()

  var reads, writes : isl.UnionMap = null
  var deadAfterScop : isl.UnionSet = null

  object deps {
    var flow : isl.UnionMap = null
    var antiOut : isl.UnionMap = null
    var input : isl.UnionMap = null

    def validity() : isl.UnionMap = {
      return Isl.simplify(flow.union(antiOut))
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
      njuLoopVars += "_i" + i
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
