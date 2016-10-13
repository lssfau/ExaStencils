package exastencils.deprecated.l4

import scala.collection.mutable._

import exastencils.base.l4.L4_Assignment
import exastencils.baseExt.l4._
import exastencils.core.collectors.L4CommCollector
import exastencils.datastructures._
import exastencils.field.l4.L4_FieldAccess
import exastencils.l4.L4_Communicate

/// L4_WrapFieldOps

@deprecated("to be re-integrated", "13.10.16")
object L4_WrapFieldOps extends DefaultStrategy("Add communication and loops to L4 statements") {
  this += new Transformation("Search and wrap", {
    case assignment @ L4_Assignment(lhs : L4_FieldAccess, rhs, op) => {
      CollectCommInformation.applyStandalone(assignment)

      val commStatements = CollectCommInformation.commCollector.communicates.map(comm =>
        L4_Communicate(comm._1, "both", List(/* FIXME: add radius */), None)).toList

      L4_LoopOverFragments(List(
        L4_LoopOverField(lhs, None, false, None, None, None, None, List(assignment), None, commStatements, List())),
        None)
    }

    // FIXME: handle reductions
    // FIXME: handle stencil fields
    // FIXME: handle region loops
  }, false /* recursion must be switched of due to wrapping mechanism */)

  object CollectCommInformation extends DefaultStrategy("Collecting information relevant for adding communication statements") {
    var commCollector = new L4CommCollector(HashMap())
    register(commCollector)

    this += new Transformation("Collect", { // FIXME: add visitor strategy defining dummy trafo?
      case n : Node => n
    })
  }

}
