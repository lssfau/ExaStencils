package exastencils.waLBerla.ir.replacements

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.domain.ir.IR_IV_NeighborIsValid
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest

object IR_WaLBerlaReplaceDomainBoundaryConditions extends DefaultStrategy("Replace domain boundary cond") {
  val neighborDirs : ListBuffer[Array[Int]] = ListBuffer()
  for (dim <- 0 until Knowledge.dimensionality) {
    neighborDirs += Array.fill(dim)(0) ++ Array(-1) ++ Array.fill(Knowledge.dimensionality - dim - 1)(0)
    neighborDirs += Array.fill(dim)(0) ++ Array(+1) ++ Array.fill(Knowledge.dimensionality - dim - 1)(0)
  }

  this += Transformation("..", {
    case _ @ IR_Negation(IR_IV_NeighborIsValid(_, neighIdx : IR_IntegerConstant, _)) =>
      IR_WaLBerlaBlockForest().isAtDomainBorder(neighborDirs(neighIdx.v.toInt))
  })
}
