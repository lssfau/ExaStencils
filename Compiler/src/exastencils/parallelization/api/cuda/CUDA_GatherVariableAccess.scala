package exastencils.parallelization.api.cuda

import scala.collection.mutable

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_DimToString

object CUDA_GatherVariableAccess extends QuietDefaultStrategy("Gather local VariableAccess nodes") {
  var accesses = mutable.HashMap[String, IR_VariableAccess]()
  var ignoredAccesses = mutable.SortedSet[String]()

  def clear() = {
    accesses = mutable.HashMap[String, IR_VariableAccess]()
    ignoredAccesses = (0 to Knowledge.dimensionality + 2 /* FIXME: find a way to determine max dimensionality */).map(dim => IR_DimToString(dim)).to[mutable.SortedSet]
  }

  this += new Transformation("Searching", {
    case decl : IR_VariableDeclaration                                        =>
      ignoredAccesses += decl.name
      decl
    case access : IR_VariableAccess if !ignoredAccesses.contains(access.name) =>
      accesses.put(access.name, access)
      access
  }, false)
}
