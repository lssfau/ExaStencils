package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_RealDatatype
import exastencils.baseExt.l4.L4_VectorDatatype
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.l4.L4_DomainCollection
import exastencils.logger.Logger

// FIXME: move this information to the specialized grid classes and only set up virtual fields that are sensible for the chosen grid type

/// L4_PrepareVirtualFieldDeclaration

object L4_PrepareVirtualFieldDeclarations extends DefaultStrategy("Prepare knowledge for L4 virtual fields") {
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
        L4_VirtualFieldCollection.addDeclared(field, level)
      }))

    super.apply(applyAtNode)

    Logger.debug(s"Added $cnt virtual fields")
  }
}

/// L4_ProcessVirtualFieldDeclarations

object L4_ProcessVirtualFieldDeclarations extends DefaultStrategy("Integrate L4 virtual field declarations with knowledge") {
  override def apply(applyAtNode : Option[Node] = None) = {
    val domain = L4_DomainCollection.getByIdentifier("global").get
    Knowledge.levels.foreach(level => {
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_nodePosAsVec", level, domain, L4_VectorDatatype(L4_RealDatatype, Knowledge.dimensionality), "node"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_nodePosition_x", level, domain, L4_RealDatatype, "node"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_nodePosition_y", level, domain, L4_RealDatatype, "node"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_nodePosition_z", level, domain, L4_RealDatatype, "node"))

      L4_VirtualFieldCollection.add(L4_VirtualField("vf_cellCenAsVec", level, domain, L4_VectorDatatype(L4_RealDatatype, Knowledge.dimensionality), "cell"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_cellCenter_x", level, domain, L4_RealDatatype, "cell"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_cellCenter_y", level, domain, L4_RealDatatype, "cell"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_cellCenter_z", level, domain, L4_RealDatatype, "cell"))

      L4_VirtualFieldCollection.add(L4_VirtualField("vf_boundaryCoordAsVec", level, domain, L4_VectorDatatype(L4_RealDatatype, Knowledge.dimensionality), "unknown"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_boundaryCoord_x", level, domain, L4_RealDatatype, "unknown"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_boundaryCoord_y", level, domain, L4_RealDatatype, "unknown"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_boundaryCoord_z", level, domain, L4_RealDatatype, "unknown"))

      L4_VirtualFieldCollection.add(L4_VirtualField("vf_gridWidth_x", level, domain, L4_RealDatatype, "cell"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_gridWidth_y", level, domain, L4_RealDatatype, "cell"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_gridWidth_z", level, domain, L4_RealDatatype, "cell"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_cellWidth_x", level, domain, L4_RealDatatype, "cell"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_cellWidth_y", level, domain, L4_RealDatatype, "cell"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_cellWidth_z", level, domain, L4_RealDatatype, "cell"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_cellVolume", level, domain, L4_RealDatatype, "cell"))

      L4_VirtualFieldCollection.add(L4_VirtualField("vf_stagCVWidth_x", level, domain, L4_RealDatatype, "face_x"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_stagCVWidth_y", level, domain, L4_RealDatatype, "face_y"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_stagCVWidth_z", level, domain, L4_RealDatatype, "face_z"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_xStagCellVolume", level, domain, L4_RealDatatype, "face_x"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_yStagCellVolume", level, domain, L4_RealDatatype, "face_y"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_zStagCellVolume", level, domain, L4_RealDatatype, "face_z"))

      L4_VirtualFieldCollection.add(L4_VirtualField("vf_cellCenterToFace_x", level, domain, L4_RealDatatype, "cell"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_cellCenterToFace_y", level, domain, L4_RealDatatype, "cell"))
      L4_VirtualFieldCollection.add(L4_VirtualField("vf_cellCenterToFace_z", level, domain, L4_RealDatatype, "cell"))
    })

    super.apply(applyAtNode)
  }
}
