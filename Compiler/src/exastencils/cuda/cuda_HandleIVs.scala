package exastencils.cuda

import scala.collection._

import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.datastructures._

/// CUDA_GatherIVs

object CUDA_GatherIVs extends QuietDefaultStrategy("Gather local InternalVariable nodes") {
  var ivAccesses = mutable.HashMap[String, IR_InternalVariable]()

  this += new Transformation("Searching", {
    case iv : IR_InternalVariable =>
      ivAccesses.put(iv.prettyprint, iv)
      iv
  }, false)
}

/// CUDA_ReplaceIVs

object CUDA_ReplaceIVs extends QuietDefaultStrategy("Replace local InternalVariable nodes") {
  var ivAccesses = mutable.HashMap[String, IR_InternalVariable]()

  this += new Transformation("Searching", {
    case iv : IR_InternalVariable =>
      val ivAccess = ivAccesses.find(_._2 == iv).get // TODO: improve performance
      IR_VariableAccess(ivAccess._1, ivAccess._2.resolveDatatype)
  })
}

