package exastencils.waLBerla.l4.field

import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.l4.L4_ConstIndex
import exastencils.base.l4.L4_Datatype
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.ir.IR_FieldLayoutPerDim
import exastencils.field.l4.L4_FieldLayout
import exastencils.fieldlike.l4.L4_FieldLikeLayout
import exastencils.fieldlike.l4.L4_FieldLikeLayoutAccess
import exastencils.grid.l4.L4_AtCellCenter
import exastencils.grid.l4.L4_Localization
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldLayout

case class L4_WaLBerlaFieldLayout(
    var name : String, // will be used to find the layout
    var level : Int, // the level the field lives on
    var numDimsGrid : Int, // the number of dimensions of the grid
    var datatype : L4_Datatype, // data type of the field
    var waLBerlaLayout : String,
    var ghostLayers : L4_ConstIndex,
    var communicatesGhosts : Boolean,
    var duplicateLayers : L4_ConstIndex,
    var communicatesDuplicated : Boolean,
    var innerPoints : L4_ConstIndex
) extends L4_FieldLikeLayout[IR_WaLBerlaFieldLayout] {

  def localization : L4_Localization = L4_AtCellCenter

  override def createDuplicate() : L4_WaLBerlaFieldLayout = {
    L4_WaLBerlaFieldLayout(name, level, numDimsGrid, Duplicate(datatype), waLBerlaLayout,
      Duplicate(ghostLayers), communicatesGhosts, Duplicate(duplicateLayers), communicatesDuplicated, innerPoints)
  }

  if (!List("zyxf", "fzyx").contains(waLBerlaLayout))
    Logger.error(s"""Invalid waLBerla data layout : $waLBerlaLayout. Only \"zyxf\" or \"fzyx\" available.""")

  override def prettyprintDecl(out : PpStream) = {
    out << "waLBerla Layout " << name << "< "
    out << datatype << ", "
    out << s"\'$waLBerlaLayout\'"
    out << " >" << "@(" << level << ") {\n"
    out << "duplicateLayers = " << duplicateLayers << (if (communicatesDuplicated) " with communication\n" else "\n")
    out << "ghostLayers = " << ghostLayers << (if (communicatesGhosts) " with communication\n" else "\n")
    out << "}"
  }

  // TODO: remove
  def toFieldLayout = L4_FieldLayout(name, level, numDimsGrid, datatype, localization, ghostLayers, communicatesGhosts, duplicateLayers, communicatesDuplicated, innerPoints)

  override def progressImpl() : IR_WaLBerlaFieldLayout = {
    // use data type after progressing due to possible vec-mat-promotion
    val progDatatype = datatype.progress

    // determine full data dimensionality
    val numDimsData = numDimsGrid + progDatatype.dimensionality

    // layouts per dim
    var layouts = Array[IR_FieldLayoutPerDim]()
    layouts ++= (0 until numDimsGrid).map(dim => IR_FieldLayoutPerDim(0, ghostLayers(dim), duplicateLayers(dim), innerPoints(dim), duplicateLayers(dim), ghostLayers(dim), 0))
    if (numDimsData > numDimsGrid) layouts ++= progDatatype.getSizeArray.map(size => IR_FieldLayoutPerDim(0, 0, 0, size, 0, 0, 0))

    val useFixedLayoutSizes = Knowledge.waLBerla_useGridFromExa && waLBerlaLayout == "fzyx"

    val dummyRefOffset = IR_ExpressionIndex(Array.fill(numDimsData)(0))

    val ret = IR_WaLBerlaFieldLayout(name, level, numDimsGrid, numDimsData, useFixedLayoutSizes, dummyRefOffset,
      progDatatype, layouts, waLBerlaLayout, communicatesDuplicated, communicatesGhosts)

    // there are two different approaches to handle the "referenceOffset" to the first inner iteration point:
    // 1. by using iteration spaces from waLBerla -> "referenceOffset" is set to zero and is automatically handled by waLBerla's accessors
    // 2. by using fixed iteration spaces from exastencils -> "referenceOffset" deduced from specified layouts per dim and handled by a proxy "IR_FieldLayout" instance
    if (useFixedLayoutSizes)
      ret.updateDefReferenceOffset()

    ret
  }

  override def toLayoutAccess : L4_FieldLikeLayoutAccess[IR_WaLBerlaFieldLayout] = L4_WaLBerlaFieldLayoutAccess(this)
}
