package exastencils.grid.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_DefaultVirtualFields extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/grid/|LAYER_LC|/|LAYER_UC|_DefaultVirtualFields.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.grid.|LAYER_LC|

import scala.collection.mutable.ListBuffer

import exastencils.base.|LAYER_LC|.|LAYER_UC|_RealDatatype
import exastencils.baseExt.|LAYER_LC|.|LAYER_UC|_VectorDatatype
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.|LAYER_LC|.|LAYER_UC|_DomainCollection
import exastencils.logger.Logger

// FIXME: move this information to the specialized grid classes and only set up virtual fields that are sensible for the chosen grid type

/// |LAYER_UC|_PrepareVirtualFieldDeclaration

object |LAYER_UC|_PrepareVirtualFieldDeclarations extends DefaultStrategy("Prepare knowledge for |LAYER_UC| virtual fields") {
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
        |LAYER_UC|_VirtualFieldCollection.addDeclared(field, level)
      }))

    super.apply(applyAtNode)

    Logger.debug(s"Added $cnt virtual fields")
  }
}

/// |LAYER_UC|_ProcessVirtualFieldDeclarations

object |LAYER_UC|_ProcessVirtualFieldDeclarations extends DefaultStrategy("Integrate |LAYER_UC| virtual field declarations with knowledge") {
  override def apply(applyAtNode : Option[Node] = None) = {
    val domain = |LAYER_UC|_DomainCollection.getByIdentifier("global").get
    Knowledge.levels.foreach(level => {
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_nodePosAsVec", level, domain, |LAYER_UC|_VectorDatatype(|LAYER_UC|_RealDatatype, Knowledge.dimensionality), "node"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_nodePosition_x", level, domain, |LAYER_UC|_RealDatatype, "node"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_nodePosition_y", level, domain, |LAYER_UC|_RealDatatype, "node"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_nodePosition_z", level, domain, |LAYER_UC|_RealDatatype, "node"))

      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_cellCenAsVec", level, domain, |LAYER_UC|_VectorDatatype(|LAYER_UC|_RealDatatype, Knowledge.dimensionality), "cell"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_cellCenter_x", level, domain, |LAYER_UC|_RealDatatype, "cell"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_cellCenter_y", level, domain, |LAYER_UC|_RealDatatype, "cell"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_cellCenter_z", level, domain, |LAYER_UC|_RealDatatype, "cell"))

      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_boundaryCoordAsVec", level, domain, |LAYER_UC|_VectorDatatype(|LAYER_UC|_RealDatatype, Knowledge.dimensionality), "unknown"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_boundaryCoord_x", level, domain, |LAYER_UC|_RealDatatype, "unknown"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_boundaryCoord_y", level, domain, |LAYER_UC|_RealDatatype, "unknown"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_boundaryCoord_z", level, domain, |LAYER_UC|_RealDatatype, "unknown"))

      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_gridWidth_x", level, domain, |LAYER_UC|_RealDatatype, "cell"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_gridWidth_y", level, domain, |LAYER_UC|_RealDatatype, "cell"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_gridWidth_z", level, domain, |LAYER_UC|_RealDatatype, "cell"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_cellWidth_x", level, domain, |LAYER_UC|_RealDatatype, "cell"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_cellWidth_y", level, domain, |LAYER_UC|_RealDatatype, "cell"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_cellWidth_z", level, domain, |LAYER_UC|_RealDatatype, "cell"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_cellVolume", level, domain, |LAYER_UC|_RealDatatype, "cell"))

      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_stagCVWidth_x", level, domain, |LAYER_UC|_RealDatatype, "face_x"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_stagCVWidth_y", level, domain, |LAYER_UC|_RealDatatype, "face_y"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_stagCVWidth_z", level, domain, |LAYER_UC|_RealDatatype, "face_z"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_xStagCellVolume", level, domain, |LAYER_UC|_RealDatatype, "face_x"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_yStagCellVolume", level, domain, |LAYER_UC|_RealDatatype, "face_y"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_zStagCellVolume", level, domain, |LAYER_UC|_RealDatatype, "face_z"))

      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_cellCenterToFace_x", level, domain, |LAYER_UC|_RealDatatype, "cell"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_cellCenterToFace_y", level, domain, |LAYER_UC|_RealDatatype, "cell"))
      |LAYER_UC|_VirtualFieldCollection.add(|LAYER_UC|_VirtualField("vf_cellCenterToFace_z", level, domain, |LAYER_UC|_RealDatatype, "cell"))
    })

    super.apply(applyAtNode)
  }
}
"""
  }
}
