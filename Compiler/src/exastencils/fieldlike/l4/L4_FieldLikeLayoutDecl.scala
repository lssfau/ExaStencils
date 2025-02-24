package exastencils.fieldlike.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.config.Knowledge
import exastencils.field.l4.L4_FieldLayoutOption
import exastencils.fieldlike.ir.IR_FieldLikeLayout
import exastencils.grid.l4._
import exastencils.knowledge.l4.L4_LeveledKnowledgeDecl

trait L4_FieldLikeLayoutDecl[L4_LayoutType <: L4_FieldLikeLayout[IR_LayoutType], IR_LayoutType <: IR_FieldLikeLayout] extends L4_LeveledKnowledgeDecl {
  def name : String
  def levels : Option[L4_DeclarationLevelSpecification]
  def datatype : L4_Datatype
  def localization : L4_Localization
  def options : ListBuffer[L4_FieldLayoutOption]

  def composeLayout(level : Int) : L4_LayoutType

  val numDimsGrid = Knowledge.dimensionality // TODO: adapt for edge data structures

  def associatedCollection : L4_FieldLikeLayoutCollection[L4_LayoutType, IR_LayoutType]

  def evalFieldLayoutValue(optionName : String) : L4_ConstIndex = {
    val option = options.find(_.name == optionName)
    if (option.isDefined)
      option.get.value
    else
      L4_FieldLikeLayout.getDefaultValue(optionName, localization)
  }

  def evalFieldLayoutBoolean(optionName : String) : Boolean = {
    val option = options.find(_.name == optionName)
    if (option.isDefined)
      option.get.hasCommunication
    else
      L4_FieldLikeLayout.getDefaultBoolean(optionName, localization)
  }

  // determine number of inner points
  def evalFieldLayoutInnerPoints(level : Int, numDup : L4_ConstIndex, numGhost : L4_ConstIndex) : L4_ConstIndex = {
    val providedInnerPoints = options.find(_.name == "innerPoints")
    if (providedInnerPoints.isDefined) {
      // user specified values are available -> use those
      providedInnerPoints.get.value
    } else {
      // attempt automatic deduction - TODO: adapt for edge data structures
      localization match {
        case L4_AtNode       => L4_ConstIndex((0 until numDimsGrid).map(dim => ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 1) - 2 * numDup(dim)).toArray)
        case L4_AtCellCenter => L4_ConstIndex((0 until numDimsGrid).map(dim => ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 0) - 2 * numDup(dim)).toArray)

        case L4_AtFaceCenter(faceDim) => L4_ConstIndex((0 until numDimsGrid).map(dim =>
          if (dim == faceDim)
            ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 1) - 2 * numDup(dim)
          else
            ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 0) - 2 * numDup(dim)).toArray)

        case L4_HACK_OtherLocalization("edge_node") => L4_ConstIndex((0 until numDimsGrid).map(dim =>
          if (0 == dim)
            ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 1) - 2 * numDup(dim)
          else
            0).toArray)
        case L4_HACK_OtherLocalization("edge_cell") => L4_ConstIndex((0 until numDimsGrid).map(dim =>
          if (0 == dim)
            ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 0) - 2 * numDup(dim)
          else
            0).toArray)
      }
    }
  }
}
