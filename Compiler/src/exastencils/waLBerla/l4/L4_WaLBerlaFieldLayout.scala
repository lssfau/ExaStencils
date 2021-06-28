package exastencils.waLBerla.l4

import exastencils.base.l4.L4_ConstIndex
import exastencils.base.l4.L4_Datatype
import exastencils.core.Duplicate
import exastencils.field.ir.IR_FieldLayoutPerDim
import exastencils.field.l4.L4_FieldLayout
import exastencils.grid.l4.L4_AtCellCenter
import exastencils.grid.l4.L4_Localization
import exastencils.knowledge.l4.L4_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.IR_WaLBerlaFieldLayout

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
) extends L4_LeveledKnowledgeObject[IR_WaLBerlaFieldLayout] {

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

  def toFieldLayout = L4_FieldLayout(name, level, numDimsGrid, datatype, localization, ghostLayers, communicatesGhosts, duplicateLayers, communicatesDuplicated, innerPoints)

  override def progressImpl() : IR_WaLBerlaFieldLayout = {
    // use data type after progressing due to possible vec-mat-promotion
    val progDatatype = datatype.progress

    // determine full data dimensionality
    val numDimsData = numDimsGrid + progDatatype.dimensionality

    // layouts per dim
    var layouts = Array[IR_FieldLayoutPerDim]()
    if (ghostLayers.sum != 0 || duplicateLayers.sum != 0 || innerPoints.sum != numDimsGrid) {
      // throw error if options were set for waLBerla fields
      Logger.warn(
        s"""Invalid ghost/duplicate/inner layer configuration for waLBerla field layouts. Only set these values for Exa-internal fields, i.e. for levels other than the maximum.
          | Config is: ghost = ${ghostLayers.prettyprint()}, dup = ${duplicateLayers.prettyprint()}, inner = ${innerPoints.prettyprint()}
          |""".stripMargin)
    }
    layouts ++= (0 until numDimsGrid).map(dim => IR_FieldLayoutPerDim(0, ghostLayers(dim), duplicateLayers(dim), innerPoints(dim), duplicateLayers(dim), ghostLayers(dim), 0))
    if (numDimsData > numDimsGrid) layouts ++= progDatatype.getSizeArray.map(size => IR_FieldLayoutPerDim(0, 0, 0, size, 0, 0, 0))

    IR_WaLBerlaFieldLayout(name, level, numDimsGrid, numDimsData, progDatatype, layouts, waLBerlaLayout, communicatesDuplicated, communicatesGhosts)
  }
}
