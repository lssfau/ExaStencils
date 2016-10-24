package exastencils.communication.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.boundary.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_DimToString
import exastencils.field.l4._
import exastencils.logger.Logger

/// L4_AdaptFieldLayoutsForComm

object L4_AdaptFieldLayoutsForComm extends DefaultStrategy("Adapt field layouts to take communication patterns into account") {
  var collector = new L4_FieldAccessRangeCollector()

  override def apply(node : Option[Node] = None) = {
    collector.reset()
    this.register(collector)
    super.apply(node)
    this.unregister(collector)
    collector.adaptNodeBasedFields()
    actuallyAdapt()
  }

  override def applyStandalone(node : Node) = {
    collector.reset()
    this.register(collector)
    super.applyStandalone(node)
    this.unregister(collector)
    collector.adaptNodeBasedFields()
    actuallyAdapt()
  }

  def actuallyAdapt() = {
    var unreferencedFields = ListBuffer[L4_Field]()

    val readExtentMin = collector.readExtentMin.map(e => (e._1.field, e._2))
    val readExtentMax = collector.readExtentMax.map(e => (e._1.field, e._2))
    val writeExtentMin = collector.writeExtentMin.map(e => (e._1.field, e._2))
    val writeExtentMax = collector.writeExtentMax.map(e => (e._1.field, e._2))

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
          val localization = defLayout.discretization.toLowerCase
          if ("node" == localization || s"face_${ IR_DimToString(i) }" == localization) {
            // node type localization doesn't require ghost layers for boundary handling - apart from Neumann
            field.boundary match {
              case L4_NeumannBC(order) => {
                numGhostLayersLeft(i) = math.max(numGhostLayersLeft(i), 1)
                numGhostLayersRight(i) = math.max(numGhostLayersRight(i), 1)
              }
              case _                   =>
            }
          } else if ("cell" == localization || "face_x" == localization || "face_y" == localization || "face_z" == localization) {
            // cell type localization always requires (at least) on ghost layer for implementing boundary conditions
            numGhostLayersLeft(i) = math.max(numGhostLayersLeft(i), 1)
            numGhostLayersRight(i) = math.max(numGhostLayersRight(i), 1)
          } else {
            Logger.warn(s"Encountered unknown localization: $localization")
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
        val newLayout = Duplicate(defLayout)
        newLayout.name = newLayoutName
        val numGhostLayers = (numGhostLayersLeft, numGhostLayersRight).zipped.map(math.max)
        newLayout.ghostLayers = L4_ConstIndex(numGhostLayers)
        newLayout.communicatesGhosts = numGhostLayers.count(_ != 0) > 0
        // FIXME: how to determine if duplicates should communicate? activate by default?
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
