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

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.boundary.l4._
import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.grid.l4._
import exastencils.logger.Logger

/// L4_AdaptFieldLayoutsForComm

// TODO: Use FieldLikeCollections instead or FieldCollection

object L4_AdaptFieldLayoutsForComm extends DefaultStrategy("Adapt field layouts to take communication patterns into account") {
  var collector = new L4_FieldAccessRangeCollector()

  override def apply(node : Option[Node] = None) = {
    collector.reset()
    this.register(collector)
    super.apply(node)
    this.unregister(collector)
    collector.adaptNodeBasedFields()
    actuallyAdapt()
    collector.reset()
  }

  override def applyStandalone(node : Node) = {
    collector.reset()
    this.register(collector)
    super.applyStandalone(node)
    this.unregister(collector)
    collector.adaptNodeBasedFields()
    actuallyAdapt()
    collector.reset()
  }

  def actuallyAdapt() = {
    var unreferencedFields = ListBuffer[L4_Field]()

    // re-map read and write extent maps, ie ignore slots
    val readExtentMin = HashMap[L4_Field, Array[Int]]()
    collector.readExtentMin.foreach(e => {
      if (readExtentMin.contains(e._1.field))
        readExtentMin(e._1.field) = (readExtentMin(e._1.field), e._2).zipped.map(math.min)
      else
        readExtentMin.put(e._1.field, e._2)
    })

    val readExtentMax = HashMap[L4_Field, Array[Int]]()
    collector.readExtentMax.foreach(e => {
      if (readExtentMax.contains(e._1.field))
        readExtentMax(e._1.field) = (readExtentMax(e._1.field), e._2).zipped.map(math.max)
      else
        readExtentMax.put(e._1.field, e._2)
    })

    val writeExtentMin = HashMap[L4_Field, Array[Int]]()
    collector.writeExtentMin.foreach(e => {
      if (writeExtentMin.contains(e._1.field))
        writeExtentMin(e._1.field) = (writeExtentMin(e._1.field), e._2).zipped.map(math.min)
      else
        writeExtentMin.put(e._1.field, e._2)
    })

    val writeExtentMax = HashMap[L4_Field, Array[Int]]()
    collector.writeExtentMax.foreach(e => {
      if (writeExtentMax.contains(e._1.field))
        writeExtentMax(e._1.field) = (writeExtentMax(e._1.field), e._2).zipped.map(math.max)
      else
        writeExtentMax.put(e._1.field, e._2)
    })

    for (field <- L4_FieldCollection.objects) {
      val defLayout = field.fieldLayout

      val fieldIsRead = readExtentMin.contains(field)
      val fieldIsWritten = writeExtentMin.contains(field)
      if (!fieldIsRead)
        Logger.warn(s"Found l4 field without read access: ${ field.name } on level ${ field.level }")
      if (!fieldIsWritten)
        Logger.warn(s"Found l4 field without write access: ${ field.name } on level ${ field.level }")

      if (!fieldIsRead && !fieldIsWritten)
        unreferencedFields += field

      val numGhostLayersLeft = (fieldIsRead, fieldIsWritten) match {
        case (true, false)  => readExtentMin(field).map(-1 * _)
        case (false, true)  => writeExtentMin(field).map(-1 * _)
        case (true, true)   => (writeExtentMin(field), readExtentMin(field)).zipped.map(math.min).map(-1 * _)
        case (false, false) => Array.fill(field.numDimsGrid)(0)
      }
      val numGhostLayersRight = (fieldIsRead, fieldIsWritten) match {
        case (true, false)  => readExtentMax(field)
        case (false, true)  => writeExtentMax(field)
        case (true, true)   => (writeExtentMax(field), readExtentMax(field)).zipped.map(math.max)
        case (false, false) => Array.fill(field.numDimsGrid)(0)
      }

      // adapt for bc's of field if required
      if (L4_NoBC != field.boundary) { // TODO: update for new bc classes; Neumann for node; warn for fcts
        for (i <- numGhostLayersLeft.indices) {
          defLayout.localization match {
            case L4_AtNode | L4_AtFaceCenter(`i`) =>
              // node type localization doesn't require ghost layers for all boundary handling cases
              field.boundary match {
                case L4_NeumannBC(order) =>
                  numGhostLayersLeft(i) = math.max(numGhostLayersLeft(i), 1)
                  numGhostLayersRight(i) = math.max(numGhostLayersRight(i), 1)

                case _ : L4_FunctionBC => // no info -> assume requirement
                  numGhostLayersLeft(i) = math.max(numGhostLayersLeft(i), 1)
                  numGhostLayersRight(i) = math.max(numGhostLayersRight(i), 1)

                case _ : L4_DirichletBC => // nothing to do

                case other => Logger.warn(s"Unsupported boundary condition $other")
              }

            case L4_AtCellCenter | L4_AtFaceCenter(_) =>
              // cell type localization always requires (at least) on ghost layer for implementing boundary conditions
              numGhostLayersLeft(i) = math.max(numGhostLayersLeft(i), 1)
              numGhostLayersRight(i) = math.max(numGhostLayersRight(i), 1)

            case other => Logger.warn(s"Encountered unknown localization: $other")
          }
        }
      }

      val newLayoutName = s"${ defLayout.name }__${ numGhostLayersLeft.mkString("_") }__${ numGhostLayersRight.mkString("_") }"

      // check if layout already exists
      if (L4_FieldLayoutCollection.exists(newLayoutName, field.level)) {
        // assign layout to field
        field.fieldLayout = L4_FieldLayoutCollection.getByIdentifier(newLayoutName, field.level).get
      } else {
        // layout doesn't exist yet -> create it
        val newLayout = defLayout.createDuplicate()
        newLayout.name = newLayoutName
        val numGhostLayers = (numGhostLayersLeft, numGhostLayersRight).zipped.map(math.max)
        newLayout.ghostLayers = L4_ConstIndex(numGhostLayers)
        newLayout.communicatesGhosts = numGhostLayers.exists(_ != 0)
        // FIXME: how to determine if duplicates should communicate? activate by default?
        newLayout.communicatesDuplicated = newLayout.duplicateLayers.indices.exists(_ != 0)
        L4_FieldLayoutCollection.add(newLayout)

        // assign layout to field
        field.fieldLayout = newLayout
      }
    }

    // cleanup - remove unused fields and field layouts // TODO: control via knowledge parameter
    if (false) {
      L4_FieldCollection.objects = L4_FieldCollection.objects.filter(field => !unreferencedFields.contains(field))
      L4_FieldLayoutCollection.objects = L4_FieldLayoutCollection.objects.filter(layout => L4_FieldCollection.objects.exists(field => field.fieldLayout == layout && field.level == layout.level))
    }
  }

  this += new Transformation("Collect", PartialFunction.empty)
}
