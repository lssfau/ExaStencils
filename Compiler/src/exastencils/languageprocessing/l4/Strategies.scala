package exastencils.languageprocessing.l4

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core.collectors.L4CommCollector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.l4._
import exastencils.knowledge

object CollectCommInformation extends DefaultStrategy("Collecting information relevant for adding communication statements") {
  var commCollector : L4CommCollector = new L4CommCollector(HashMap())

  override def apply(node : Option[Node] = None) = {
    commCollector.reset()
    this.register(commCollector)
    super.apply(node)
    this.unregister(commCollector)
  }

  override def applyStandalone(node : Node) = {
    commCollector.reset()
    this.register(commCollector)
    super.applyStandalone(node)
    this.unregister(commCollector)
  }

  this += new Transformation("Collect", { // FIXME: add visitor strategy defining dummy trafo?
    case n : Node => n
  })
}

object ResolveL4Constants extends DefaultStrategy("Resolving constants on L4") {
  this += new Transformation("Search and replace", {
    case BasicAccess("PI") | BasicAccess("M_PI") | BasicAccess("Pi") => FloatConstant(math.Pi)

    case LeveledAccess("hx", SingleLevelSpecification(level))        => FloatConstant(knowledge.Knowledge.discr_hx(level - knowledge.Knowledge.minLevel))
    case LeveledAccess("hy", SingleLevelSpecification(level))        => FloatConstant(knowledge.Knowledge.discr_hy(level - knowledge.Knowledge.minLevel))
    case LeveledAccess("hz", SingleLevelSpecification(level))        => FloatConstant(knowledge.Knowledge.discr_hz(level - knowledge.Knowledge.minLevel))

    // TODO: progress values from globals
  })
}

object WrapL4FieldOpsStrategy extends DefaultStrategy("Adding communcation and loops to L4 statements") {
  this += new Transformation("Search and wrap", {
    case assignment @ AssignmentStatement(lhs : FieldAccess, rhs, op) => {
      CollectCommInformation.applyStandalone(assignment)

      var statements : ListBuffer[Statement] =
        CollectCommInformation.commCollector.communicates.map(comm =>
          CommunicateStatement(comm._1, "both", List( /* FIXME: add radius */ )) : Statement).to[ListBuffer]

      statements += LoopOverFragmentsStatement(List(
        LoopOverPointsStatement(lhs, false, None, None, None, None, List(assignment), None)),
        None)

      statements
    }

    // FIXME: handle reductions
    // FIXME: handle stencil fields
  }, false /* recursion must be switched of due to wrapping mechanism */ )
}
