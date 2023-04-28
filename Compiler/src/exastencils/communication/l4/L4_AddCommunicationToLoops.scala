//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.communication.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Statement
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_LoopOverField
import exastencils.boundary.l4._
import exastencils.communication.l4.L4_FieldAccessRangeCollector.L4_FieldWithSlot
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.prettyprinting.PpStream

/// L4_AddCommunicationToLoops

object L4_AddCommunicationToLoops extends DefaultStrategy("Add communication statements to loops") {

  // implements a pull strategy
  this += new Transformation("Add communication to loops", {
    case loop : L4_LoopOverField =>
      L4_CollectCommInformation.applyStandalone(loop)
      val collector = L4_CollectCommInformation.collector

      case class L4_Comment(var comment : String) extends L4_Statement {
        override def prettyprint(out : PpStream) : Unit = out << "/* " << comment << " */"
        override def progress : IR_Statement = ???
      }
      var finalStmts = ListBuffer[L4_Statement]()

      // find all fields read outside the iteration space
      var fieldsToConsider = ListBuffer[L4_FieldAccessRangeCollector.L4_FieldWithSlot]()
      for (fieldData <- collector.readExtentMax)
        if (fieldData._2.exists(_ > 0))
          fieldsToConsider += fieldData._1
      for (fieldData <- collector.readExtentMin)
        if (fieldData._2.exists(_ < 0))
          fieldsToConsider += fieldData._1

      fieldsToConsider = fieldsToConsider.distinct

      collector.readExtentMax.keys.foreach { f =>
        finalStmts += L4_Comment(f.field.name + " => " + collector.readExtentMin(f).mkString(", ") + " <-> " + collector.readExtentMax(f).mkString(", "))
      }

      // add combination fields
      fieldsToConsider = fieldsToConsider.flatMap {
        case L4_FieldWithSlot(field, slot) if L4_FieldCombinationCollection.existsInCombination(field) =>
          L4_FieldCombinationCollection.getByFieldInCombination(field).flatMap(_.fields).map(f => L4_FieldWithSlot(f, Duplicate(slot)))

        case o => ListBuffer(o)
      }.distinct

      var commStatements = ListBuffer[L4_Communicate]()

      for (field <- fieldsToConsider.sortBy(f => f.field.name + f.field.level + f.slot)) {
        var targets = ListBuffer[L4_CommunicateTarget]()
        targets += L4_CommunicateTarget("all", None, None) // FIXME: Some(L4_ConstIndex(collector.readExtentMax(field).map(math.max(_, 0)))))
        commStatements += L4_Communicate(
          L4_FieldAccess(field.field, field.slot),
          "both",
          targets.toList,
          None,
          None)
        // TODO: append potential assignment condition to communicate statement
      }

      if (false) { // append as preComms
        loop.preComms ++= commStatements
        finalStmts += loop
      } else { // prepend comm statements
        finalStmts ++= commStatements.map(s => s : L4_Statement)
        finalStmts += loop
      }

      // TODO: move to separate strategy
      for (field <- collector.writeExtentMax.keys.toList.sortBy(f => f.field.name + f.field.level + f.slot))
        if (L4_NoBC != field.boundary)
          finalStmts += L4_ApplyBC(L4_FieldAccess(field.field, field.slot))

      finalStmts

    // FIXME: handle reductions
    // FIXME: handle stencil fields
    // FIXME: handle region loops
  }, false)

  object L4_CollectCommInformation extends QuietDefaultStrategy("Collect information relevant for adding communication statements") {
    val collector = new L4_FieldAccessRangeCollector()
    register(collector)
    this.onBefore = () => this.resetCollectors()

    override def apply(node : Option[Node] = None) = {
      super.apply(node)
      collector.adaptNodeBasedFields()
    }

    override def applyStandalone(node : Node) = {
      super.applyStandalone(node)
      collector.adaptNodeBasedFields()
    }

    this += new Transformation("Collect", PartialFunction.empty)
  }

}
