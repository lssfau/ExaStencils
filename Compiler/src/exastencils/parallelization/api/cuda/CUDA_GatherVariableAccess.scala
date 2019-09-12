package exastencils.parallelization.api.cuda

import scala.collection.mutable

import exastencils.base.ir._
import exastencils.datastructures._

object CUDA_GatherVariableAccess extends QuietDefaultStrategy("Gather local VariableAccess nodes") {
  var accesses = mutable.HashMap[String, IR_VariableAccess]()
  var ignoredAccesses = mutable.SortedSet[String]()

  def clear() = {
    accesses = mutable.HashMap[String, IR_VariableAccess]()
  }

  this += new Transformation("Searching", {
    case decl : IR_VariableDeclaration =>
      ignoredAccesses += decl.name
      decl

    case access : IR_VariableAccess if !ignoredAccesses.contains(access.name) =>
      accesses.put(access.name, access)
      access
  })
}
