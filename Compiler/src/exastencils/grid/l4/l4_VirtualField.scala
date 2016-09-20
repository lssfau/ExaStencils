package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.datastructures.l4.UnresolvedAccess
import exastencils.logger.Logger

/// L4_ResolveVirtualFieldAccesses

object L4_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
  // TODO: merge this list with ir structures
  val virtualFields : ListBuffer[String] = ListBuffer(
    "vf_nodePosition_x", "vf_nodePosition_y", "vf_nodePosition_z",
    "nodePosition_x", "nodePosition_y", "nodePosition_z",

    "vf_cellCenter_x", "vf_cellCenter_y", "vf_cellCenter_z",
    "cellCenter_x", "cellCenter_y", "cellCenter_z",

    "vf_boundaryCoord_x", "vf_boundaryCoord_y", "vf_boundaryCoord_z",
    "boundaryCoord_x", "boundaryCoord_y", "boundaryCoord_z",

    "vf_gridWidth_x", "vf_gridWidth_y", "vf_gridWidth_z",
    "gridWidth_x", "gridWidth_y", "gridWidth_z",

    "vf_cellWidth_x", "vf_cellWidth_y", "vf_cellWidth_z",
    "cellWidth_x", "cellWidth_y", "cellWidth_z",

    "vf_stagCVWidth_x", "vf_stagCVWidth_y", "vf_stagCVWidth_z",
    "stagCVWidth_x", "stagCVWidth_y", "stagCVWidth_z",

    "vf_cellVolume", "vf_xStagCellVolume", "vf_yStagCellVolume", "vf_zStagCellVolume",
    "cellVolume", "xStagCellVolume", "yStagCellVolume", "zStagCellVolume",

    "vf_cellCenterToFace_x", "vf_cellCenterToFace_y", "vf_cellCenterToFace_z").map(_.toLowerCase())

  this += new Transformation("Resolve AccessSpecifications", {
    case access : UnresolvedAccess if virtualFields.contains(access.name.toLowerCase()) =>
      // resolveToVirtualFieldAccess
      if (access.dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on special field - was an offset access (@) intended?")
      if (access.slot.isDefined) Logger.warn("Discarding meaningless slot access on special field")
      L4_VirtualFieldAccess(access.name, access.level.get, access.arrayIndex, access.offset)
  })
}
