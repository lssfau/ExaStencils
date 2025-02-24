package exastencils.waLBerla.ir.replacements

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.domain.ir.IR_IV_NeighborIsValid
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest

object IR_WaLBerlaReplaceDomainBoundaryConditions extends IR_WaLBerlaReplacementStrategy("Replace domain boundary condition for waLBerla boundary kernels") {
  this += Transformation("..", {
    case _ @ IR_IfCondition(IR_Negation(IR_IV_NeighborIsValid(_, neighIdx : IR_IntegerConstant, _, _)), trueBody, falseBody) if inWaLBerlaScope(collector) =>
      val neigh = DefaultNeighbors.neighbors(neighIdx.v.toInt)
      IR_IfCondition(IR_WaLBerlaBlockForest().isAtDomainBorder(neigh.dir), trueBody, falseBody)
  })
}
