package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.grid.ir.IR_VirtualFieldAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

case class L4_VirtualFieldAccess(var name : String, var level : L4_AccessLevelSpecification, var arrayIndex : Option[Int] = None, var offset : Option[L4_ExpressionIndex] = None) extends L4_Access {
  def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (offset.isDefined) out << "@" << offset
  }

  def progress : IR_VirtualFieldAccess = {
    var numDims = Knowledge.dimensionality // TODO: resolve field info
    if (arrayIndex.isDefined) numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = IR_LoopOverDimensions.defIt(numDims)
    if (arrayIndex.isDefined)
      multiIndex(numDims - 1) = IR_IntegerConstant(arrayIndex.get)
    if (offset.isDefined) {
      var progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= IR_IntegerConstant(0)
      multiIndex += progressedOffset
    }

    IR_VirtualFieldAccess(name, IR_IntegerConstant(level.resolveLevel), multiIndex, arrayIndex)
  }
}

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

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if virtualFields.contains(access.name.toLowerCase()) =>
      // resolveToVirtualFieldAccess
      if (access.dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on special field - was an offset access (@) intended?")
      if (access.slot.isDefined) Logger.warn("Discarding meaningless slot access on special field")
      L4_VirtualFieldAccess(access.name, access.level.get, access.arrayIndex, access.offset)
  })
}
