package exastencils.stencil.l3

import exastencils.knowledge.l3.L3_LeveledKnowledgeCollection
import exastencils.stencil.l4._

object L3_OperatorCollection extends L3_LeveledKnowledgeCollection[L3_Operator, L4_Operator] {
//  def prepareFieldLayout = {
//    var requiredLayouts = HashMap[(String, Int), (Datatype, String)]()
//    for (operator <- objects)
//      operator match {
//        case op : StencilTemplateOperator => requiredLayouts +=
//          ((op.stencil.fieldLayoutName, op.level) -> (ArrayDatatype(op.stencil.resolveCoeffDataType, op.stencil.offsets.length), op.stencil.localization))
//        case _                            =>
//      }
//
//    def defIndex = Knowledge.dimensionality match {
//      case 1 => L4_Index1D(0)
//      case 2 => L4_Index2D(0, 0)
//      case 3 => L4_Index3D(0, 0, 0)
//    }
//
//    for (layout <- requiredLayouts.toList.sortBy(_._1._2).sortBy(_._1._1)) {
//      L4_FieldLayoutCollection.add(L4_FieldLayout(
//        layout._1._1, // identifier
//        layout._1._2, // level
//        layout._2._1.progress, // datatype
//        layout._2._2, // localization
//        L4_FieldLayout.default_ghostLayers(layout._2._2), // to be determined later
//        false,
//        L4_FieldLayout.default_duplicateLayers(layout._2._2),
//        false,
//        defIndex))
//    }
//  }

  def progress() = {
    for (obj <- objects)
      L4_OperatorCollection.add(obj.progress().asInstanceOf[L4_Operator]/*.asInstanceOf[L4_Operator[_ <: IR_KnowledgeObject]]*/)
  }
}
