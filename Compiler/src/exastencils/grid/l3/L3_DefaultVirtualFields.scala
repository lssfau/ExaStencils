package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_RealDatatype
import exastencils.baseExt.l3.L3_VectorDatatype
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.l3.L3_DomainCollection
import exastencils.logger.Logger

// FIXME: move this information to the specialized grid classes and only set up virtual fields that are sensible for the chosen grid type

/// L3_PrepareVirtualFieldDeclaration

object L3_PrepareVirtualFieldDeclarations extends DefaultStrategy("Prepare knowledge for L3 virtual fields") {
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
        L3_VirtualFieldCollection.addDeclared(field, level)
      }))

    super.apply(applyAtNode)

    Logger.debug(s"Added $cnt virtual fields")
  }
}

/// L3_ProcessVirtualFieldDeclarations

object L3_ProcessVirtualFieldDeclarations extends DefaultStrategy("Integrate L3 virtual field declarations with knowledge") {
  override def apply(applyAtNode : Option[Node] = None) = {
    val domain = L3_DomainCollection.getByIdentifier("global").get
    Knowledge.levels.foreach(level => {
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_nodePosAsVec", level, domain, L3_VectorDatatype(L3_RealDatatype, Knowledge.dimensionality), "node"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_nodePosition_x", level, domain, L3_RealDatatype, "node"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_nodePosition_y", level, domain, L3_RealDatatype, "node"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_nodePosition_z", level, domain, L3_RealDatatype, "node"))

      L3_VirtualFieldCollection.add(L3_VirtualField("vf_cellCenAsVec", level, domain, L3_VectorDatatype(L3_RealDatatype, Knowledge.dimensionality), "cell"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_cellCenter_x", level, domain, L3_RealDatatype, "cell"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_cellCenter_y", level, domain, L3_RealDatatype, "cell"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_cellCenter_z", level, domain, L3_RealDatatype, "cell"))

      L3_VirtualFieldCollection.add(L3_VirtualField("vf_boundaryCoordAsVec", level, domain, L3_VectorDatatype(L3_RealDatatype, Knowledge.dimensionality), "unknown"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_boundaryCoord_x", level, domain, L3_RealDatatype, "unknown"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_boundaryCoord_y", level, domain, L3_RealDatatype, "unknown"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_boundaryCoord_z", level, domain, L3_RealDatatype, "unknown"))

      L3_VirtualFieldCollection.add(L3_VirtualField("vf_gridWidth_x", level, domain, L3_RealDatatype, "cell"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_gridWidth_y", level, domain, L3_RealDatatype, "cell"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_gridWidth_z", level, domain, L3_RealDatatype, "cell"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_cellWidth_x", level, domain, L3_RealDatatype, "cell"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_cellWidth_y", level, domain, L3_RealDatatype, "cell"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_cellWidth_z", level, domain, L3_RealDatatype, "cell"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_cellVolume", level, domain, L3_RealDatatype, "cell"))

      L3_VirtualFieldCollection.add(L3_VirtualField("vf_stagCVWidth_x", level, domain, L3_RealDatatype, "face_x"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_stagCVWidth_y", level, domain, L3_RealDatatype, "face_y"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_stagCVWidth_z", level, domain, L3_RealDatatype, "face_z"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_xStagCellVolume", level, domain, L3_RealDatatype, "face_x"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_yStagCellVolume", level, domain, L3_RealDatatype, "face_y"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_zStagCellVolume", level, domain, L3_RealDatatype, "face_z"))

      L3_VirtualFieldCollection.add(L3_VirtualField("vf_cellCenterToFace_x", level, domain, L3_RealDatatype, "cell"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_cellCenterToFace_y", level, domain, L3_RealDatatype, "cell"))
      L3_VirtualFieldCollection.add(L3_VirtualField("vf_cellCenterToFace_z", level, domain, L3_RealDatatype, "cell"))
    })

    super.apply(applyAtNode)
  }
}
