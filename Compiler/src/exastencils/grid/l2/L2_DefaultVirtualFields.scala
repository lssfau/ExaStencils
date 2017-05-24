package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_RealDatatype
import exastencils.baseExt.l2.L2_VectorDatatype
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.l2.L2_DomainCollection
import exastencils.logger.Logger

// FIXME: move this information to the specialized grid classes and only set up virtual fields that are sensible for the chosen grid type

/// L2_PrepareVirtualFieldDeclaration

object L2_PrepareVirtualFieldDeclarations extends DefaultStrategy("Prepare knowledge for L2 virtual fields") {
  val fields = ListBuffer(
    "vf_nodePosAsVec", "vf_nodePosition_x", "vf_nodePosition_y", "vf_nodePosition_z",
    "vf_cellCenAsVec", "vf_cellCenter_x", "vf_cellCenter_y", "vf_cellCenter_z",

    "vf_boundaryCoordAsVec", "vf_boundaryCoord_x", "vf_boundaryCoord_y", "vf_boundaryCoord_z",

    "vf_gridWidth_x", "vf_gridWidth_y", "vf_gridWidth_z",
    "vf_cellWidth_x", "vf_cellWidth_y", "vf_cellWidth_z",
    "vf_cellVolume",

    "vf_stagCVWidth_x", "vf_stagCVWidth_y", "vf_stagCVWidth_z",
    "vf_xStagCellVolume", "vf_yStagCellVolume", "vf_zStagCellVolume",

    "vf_cellCenterToFace_x", "vf_cellCenterToFace_y", "vf_cellCenterToFace_z")

  override def apply(applyAtNode : Option[Node] = None) = {
    var cnt = 0
    Knowledge.levels.foreach(level =>
      fields.foreach(field => {
        cnt += 1
        L2_VirtualFieldCollection.addDeclared(field, level)
      }))

    super.apply(applyAtNode)

    Logger.debug(s"Added $cnt virtual fields")
  }
}

/// L2_ProcessVirtualFieldDeclarations

object L2_ProcessVirtualFieldDeclarations extends DefaultStrategy("Integrate L2 virtual field declarations with knowledge") {
  override def apply(applyAtNode : Option[Node] = None) = {
    val domain = L2_DomainCollection.getByIdentifier("global").get
    Knowledge.levels.foreach(level => {
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_nodePosAsVec", level, domain, L2_VectorDatatype(L2_RealDatatype, Knowledge.dimensionality), "node"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_nodePosition_x", level, domain, L2_RealDatatype, "node"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_nodePosition_y", level, domain, L2_RealDatatype, "node"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_nodePosition_z", level, domain, L2_RealDatatype, "node"))

      L2_VirtualFieldCollection.add(L2_VirtualField("vf_cellCenAsVec", level, domain, L2_VectorDatatype(L2_RealDatatype, Knowledge.dimensionality), "cell"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_cellCenter_x", level, domain, L2_RealDatatype, "cell"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_cellCenter_y", level, domain, L2_RealDatatype, "cell"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_cellCenter_z", level, domain, L2_RealDatatype, "cell"))

      L2_VirtualFieldCollection.add(L2_VirtualField("vf_boundaryCoordAsVec", level, domain, L2_VectorDatatype(L2_RealDatatype, Knowledge.dimensionality), "unknown"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_boundaryCoord_x", level, domain, L2_RealDatatype, "unknown"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_boundaryCoord_y", level, domain, L2_RealDatatype, "unknown"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_boundaryCoord_z", level, domain, L2_RealDatatype, "unknown"))

      L2_VirtualFieldCollection.add(L2_VirtualField("vf_gridWidth_x", level, domain, L2_RealDatatype, "cell"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_gridWidth_y", level, domain, L2_RealDatatype, "cell"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_gridWidth_z", level, domain, L2_RealDatatype, "cell"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_cellWidth_x", level, domain, L2_RealDatatype, "cell"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_cellWidth_y", level, domain, L2_RealDatatype, "cell"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_cellWidth_z", level, domain, L2_RealDatatype, "cell"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_cellVolume", level, domain, L2_RealDatatype, "cell"))

      L2_VirtualFieldCollection.add(L2_VirtualField("vf_stagCVWidth_x", level, domain, L2_RealDatatype, "face_x"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_stagCVWidth_y", level, domain, L2_RealDatatype, "face_y"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_stagCVWidth_z", level, domain, L2_RealDatatype, "face_z"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_xStagCellVolume", level, domain, L2_RealDatatype, "face_x"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_yStagCellVolume", level, domain, L2_RealDatatype, "face_y"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_zStagCellVolume", level, domain, L2_RealDatatype, "face_z"))

      L2_VirtualFieldCollection.add(L2_VirtualField("vf_cellCenterToFace_x", level, domain, L2_RealDatatype, "cell"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_cellCenterToFace_y", level, domain, L2_RealDatatype, "cell"))
      L2_VirtualFieldCollection.add(L2_VirtualField("vf_cellCenterToFace_z", level, domain, L2_RealDatatype, "cell"))
    })

    super.apply(applyAtNode)
  }
}
