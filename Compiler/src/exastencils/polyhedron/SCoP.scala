package exastencils.polyhedron

import scala.collection.mutable.HashMap

import exastencils.datastructures.Node
import exastencils.datastructures.ir.Statement

class SCoP(val root : Node, val nameToPos : HashMap[String, (isl.DimType, Int)], space : isl.Space) {

  var domain : isl.UnionSet = isl.UnionSet.empty(space)
  var schedule : isl.UnionMap = isl.UnionMap.empty(space)
  val stmts : HashMap[String, Statement] = new HashMap[String, Statement]

  var reads, writes : isl.UnionMap = isl.UnionMap.empty(space)
}
