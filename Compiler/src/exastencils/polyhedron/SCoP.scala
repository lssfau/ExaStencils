package exastencils.polyhedron

import exastencils.datastructures.Node
import scala.collection.mutable.HashMap
import exastencils.datastructures.ir.Statement

class SCoP(val root : Node) {

  var domain : isl.UnionSet = isl.UnionSet.empty(isl.Space.paramsAlloc(0))
  var schedule : isl.UnionMap = isl.UnionMap.empty(isl.Space.paramsAlloc(0))
  val stmts : HashMap[String, Statement] = new HashMap[String, Statement]
}
