package exastencils.knowledge.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.core.StateManager
import exastencils.field.l3._
import exastencils.field.l4._

/// L3_FieldCollection

object L3_FieldCollection extends L3_LeveledKnowledgeCollection[L3_Field, L4_Field] {
//  def prepareFieldLayout() = {
//    var requiredLayouts = HashMap[(String, Int), (L3_Datatype, String)]()
//    for (field <- objects)
//      requiredLayouts += ((field.fieldLayoutName, field.level) -> (field.datatype, field.localization))
//
//    def defIndex = L4_ConstIndex(Array.fill(Knowledge.dimensionality-1)(0))
//
//    for (layout <- requiredLayouts.toList.sortBy(_._1._2).sortBy(_._1._1)) {
//      L4_FieldLayoutCollection.add(L4_FieldLayout(
//        layout._1._1, // name
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

  def addInitFieldsFunction() = {
    val initStmts = ListBuffer[L3_Statement]()
    for (field <- objects)
      if (field.initial.isDefined)
        initStmts += L3_Assignment(L3_FieldAccess(field, L3_SingleLevel(field.level)), field.initial.get, "=", None)
    val fct = L3_Function("InitFields", None, L3_UnitDatatype, initStmts)
    StateManager.root_.asInstanceOf[L3_Root].nodes += fct
  }

  def progress() = {
    for (obj <- objects)
      L4_FieldCollection.add(obj.progress())
  }
}
