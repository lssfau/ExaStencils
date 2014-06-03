package exastencils.polyhedron

import scala.collection.mutable.HashMap

import exastencils.datastructures.Node
import exastencils.datastructures.ir.Statement

class SCoP(val root : Node) {

  var domain : isl.UnionSet = null
  var schedule : isl.UnionMap = null
  val stmts : HashMap[String, Statement] = new HashMap[String, Statement]

  var reads, writes : isl.UnionMap = null
}
