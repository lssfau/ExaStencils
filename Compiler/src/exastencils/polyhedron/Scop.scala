package exastencils.polyhedron

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.TreeSet

import exastencils.datastructures.Node
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.Statement
import exastencils.datastructures.ir.VariableDeclarationStatement
import isl.Conversions.convertLambdaToVoidCallback1

class Scop(val root : Node, val parallelize : Boolean) {

  var domain : isl.UnionSet = null
  var schedule : isl.UnionMap = null
  val stmts = new HashMap[String, (Statement, ArrayBuffer[String])]()
  val decls = new ListBuffer[VariableDeclarationStatement]()

  val njuLoopVars = new ArrayBuffer[String]()
  val noParDims = new TreeSet[Int]()

  var reads, writes : isl.UnionMap = null
  var deadAfterScop : isl.UnionSet = null

  object deps {
    var flow : isl.UnionMap = null
    var anti : isl.UnionMap = null
    var input : isl.UnionMap = null
    var output : isl.UnionMap = null

    def validity() : isl.UnionMap = {
      return Isl.simplify(flow.union(anti).union(output))
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
      njuLoopVars += "i" + i
      i += 1
    }
  }
}

object ScopNameMapping {

  private var count : Int = 0
  private final val id2exprMap = new HashMap[String, Expression]()
  private final val exprStr2idMap = new HashMap[String, String]()

  def id2expr(id : String) : Option[Expression] = {
    return id2exprMap.get(id)
  }

  def expr2id(expr : Expression) : String = {
    val exprStr : String = expr.cpp()
    return exprStr2idMap.getOrElse(exprStr, {
      val id : String =
        if (exprStr.size < 5)
          exprStr
        else {
          val s = "p" + count
          count += 1
          s
        }
      id2exprMap.put(id, expr)
      exprStr2idMap.put(exprStr, id)
      return id
    })
  }
}
