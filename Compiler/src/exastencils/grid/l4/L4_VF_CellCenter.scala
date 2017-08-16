package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_VectorDatatype
import exastencils.boundary.l4.L4_NoBC
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.l4.L4_Domain
import exastencils.field.l4._
import exastencils.grid.ir._

/// L4_VF_CellCenterAsVec

object L4_VF_CellCenterAsVec {
  def find(level : Int) = L4_VirtualField.findVirtualField(s"vf_cellCenter", level)
  def access(level : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level), index)
}

case class L4_VF_CellCenterAsVec(
    var level : Int,
    var domain : L4_Domain
) extends L4_VirtualFieldWithVec {

  override def name = "vf_cellCenter"
  override def knownAliases = ListBuffer("vf_cellCenterAsVec", "vf_cellCenAsVec", "vf_cellCen")
  override def localization = L4_AtCellCenter
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L4_VF_CellCenterPerDim.find(level, _)).to[ListBuffer]

  override def addAdditionalFieldsToKnowledge() = {
    if (!Knowledge.grid_isAxisAligned) {
      val layout = L4_FieldLayout(
        s"vf_cellCenterAsVec_layout", level, numDims,
        L4_VectorDatatype(L4_RealDatatype, Knowledge.dimensionality), L4_AtCellCenter,
        L4_ConstIndex(Array.fill(domain.numDims)(2)), communicatesGhosts = true,
        L4_ConstIndex(Array.fill(domain.numDims)(0)), communicatesDuplicated = true,
        L4_ConstIndex((0 until numDims).toArray.map(dim => (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim))))

      val fieldIndex = L4_FieldDecl.runningIndex
      L4_FieldDecl.runningIndex += 1

      val field = L4_Field(s"cell_center", level, fieldIndex, domain, layout, 1, L4_NoBC)

      L4_FieldLayoutCollection.add(layout)
      L4_FieldCollection.add(field)
    }
  }

  override def progressImpl() = IR_VF_CellCenterAsVec(level, domain.getProgressedObj())
}

/// L4_VF_CellCenterPerDim

object L4_VF_CellCenterPerDim {
  def find(level : Int, dim : Int) = L4_VirtualField.findVirtualField(s"vf_cellCenter_$dim", level)
  def access(level : Int, dim : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level, dim), index)
}

case class L4_VF_CellCenterPerDim(
    var level : Int,
    var domain : L4_Domain,
    var dim : Int
) extends L4_VirtualFieldPerDim {

  override def name = s"vf_cellCenter_$dim"
  override def knownAliases = ListBuffer(s"vf_cellCenter_${ L4_Localization.dimToString(dim) }", s"vf_cellCen_$dim", s"vf_cellCen_${ L4_Localization.dimToString(dim) }")
  override def localization = L4_AtCellCenter
  override def resolutionPossible = true

  override def resolve(index : L4_ExpressionIndex) = {
    if (Knowledge.grid_isUniform) {
      // nodePos + 0.5 * cellWidth
      L4_VF_NodePositionPerDim.access(level, dim, index) + 0.5 * L4_VF_CellWidthPerDim.access(level, dim, index)

    } else if (Knowledge.grid_isAxisAligned) {
      // interpolation of two node positions suffices
      0.5 * (L4_VF_NodePositionPerDim.access(level, dim, index)
        + L4_VF_NodePositionPerDim.access(level, dim, L4_GridUtil.offsetIndex(index, 1, dim)))

    } else {
      // interpolation of the current cell's corner positions

      var accesses = ListBuffer[L4_VirtualFieldAccess](L4_VF_NodePositionPerDim.access(level, dim, index))

      for (d <- 0 until numDims)
        accesses = accesses.flatMap(base => {
          val left = Duplicate(base)
          val right = Duplicate(base)
          right.index = L4_GridUtil.offsetIndex(right.index, 1, d)

          ListBuffer(left, right)
        })

      (1.0 / accesses.length) * L4_Addition(accesses.map(_.asInstanceOf[L4_Expression]))
    }
  }

  override def progressImpl() = IR_VF_CellCenterPerDim(level, domain.getProgressedObj(), dim)
}
