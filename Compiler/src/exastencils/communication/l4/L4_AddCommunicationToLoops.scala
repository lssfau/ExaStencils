package exastencils.communication.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_LoopOverField
import exastencils.boundary.l4._
import exastencils.datastructures._
import exastencils.field.l4._

/// L4_AddCommunicationToLoops

object L4_AddCommunicationToLoops extends DefaultStrategy("Add communication statements to loops") {

  // implements a pull strategy
  this += new Transformation("Add communication to loops", {
    case loop : L4_LoopOverField =>
      L4_CollectCommInformation.applyStandalone(loop)
      val collector = L4_CollectCommInformation.collector

      // find all fields read outside the iteration space
      var fieldsToConsider = ListBuffer[L4_FieldAccessRangeCollector.L4_FieldWithSlot]()
      for (fieldData <- collector.readExtentMax)
        if (fieldData._2.count(_ != 0) > 0)
          fieldsToConsider += fieldData._1

      var commStatements = ListBuffer[L4_Communicate]()

      for (field <- fieldsToConsider) {
        var targets = ListBuffer[L4_CommunicateTarget]()
        targets += L4_CommunicateTarget("ghost", None, Some(L4_ConstIndex(collector.readExtentMax(field))))
        commStatements += L4_Communicate(
          L4_FieldAccess(field.field, field.slot),
          "both",
          targets.toList,
          None)
        // TODO: append potential assignment condition to communicate statement
      }

      var finalStmts = ListBuffer[L4_Statement]()

      if (false) { // append as preComms
        loop.preComms ++= commStatements
        finalStmts += loop
      } else { // prepend comm statements
        finalStmts ++= commStatements.map(s => s : L4_Statement)
        finalStmts += loop
      }

      // TODO: move to separate strategy
      for (field <- collector.writeExtentMax.keys)
        if (L4_NoBC != field.boundary)
          finalStmts += L4_ApplyBC(L4_FieldAccess(field.field, field.slot))

      finalStmts

    // FIXME: handle reductions
    // FIXME: handle stencil fields
    // FIXME: handle region loops
  }, false)

  object L4_CollectCommInformation extends QuietDefaultStrategy("Collect information relevant for adding communication statements") {
    var collector = new L4_FieldAccessRangeCollector()

    override def apply(node : Option[Node] = None) = {
      collector.reset()
      this.register(collector)
      super.apply(node)
      this.unregister(collector)
      collector.adaptNodeBasedFields()
    }

    override def applyStandalone(node : Node) = {
      collector.reset()
      this.register(collector)
      super.applyStandalone(node)
      this.unregister(collector)
      collector.adaptNodeBasedFields()
    }

    this += new Transformation("Collect", PartialFunction.empty)
  }

}
