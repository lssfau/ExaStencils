package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable.ListBuffer

import exastencils.baseExt.ir.IR_ProcessLocalBlockLoopVariable
import exastencils.datastructures.Node
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.waLBerla.ir.grid.IR_WaLBerlaCellWidthBlockPerDim
import exastencils.waLBerla.ir.refinement.IR_WaLBerlaRefinementLevel

// implicit ordering for block-loop variables
object IR_WaLBerlaBlockLoopVariable {
  // order by type
  val byTypeOrd : Ordering[IR_WaLBerlaBlockLoopVariable] = Ordering.by {
    case _ : IR_WaLBerlaRefinementLevel          => 0
    case _ : IR_WaLBerlaCellWidthBlockPerDim     => 1
    case _ : IR_WaLBerlaNeighborHoodSectionIndex => 2
    case _ : IR_WaLBerlaNeighborHoodSectionSize  => 3
    case _                                       => 4
  }
  // order by name
  val byNameOrd : Ordering[IR_WaLBerlaBlockLoopVariable] = Ordering.by { member : IR_WaLBerlaBlockLoopVariable => member.resolveName() }

  // combine both orderings
  implicit val ord = Ordering.by { member : IR_WaLBerlaBlockLoopVariable => (member, member) }(Ordering.Tuple2(byTypeOrd, byNameOrd))
}

trait IR_WaLBerlaBlockLoopVariable extends IR_ProcessLocalBlockLoopVariable

object IR_WaLBerlaFindBlockLoopVariables extends QuietDefaultStrategy("Find accesses with refinement") {
  var blockLoopVariables : ListBuffer[IR_WaLBerlaBlockLoopVariable] = ListBuffer()

  override def applyStandalone(node : Node) : Unit = {
    blockLoopVariables.clear()
    super.applyStandalone(node)
  }

  this += Transformation("..", {
    case acc : IR_WaLBerlaBlockLoopVariable =>
      blockLoopVariables += acc
      acc
  })
}
