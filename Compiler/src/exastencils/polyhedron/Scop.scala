package exastencils.polyhedron

import scala.collection.mutable.ArrayStack
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Node
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.Statement
import exastencils.datastructures.ir.VariableDeclarationStatement

class Scop(val root : Node) {

  var domain : isl.UnionSet = null
  var schedule : isl.UnionMap = null
  val stmts = new HashMap[String, (Statement, ArrayStack[String])]
  val decls = new ListBuffer[VariableDeclarationStatement]

  var reads, writes : isl.UnionMap = null

  var deps : isl.UnionMap = null
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
