package exastencils.knowledge.l3

import scala.collection.mutable._

import exastencils.core._
import exastencils.knowledge._

/// L3_FieldCollection

//object L3_FieldCollection extends KnowledgeCollectionLeveled[Field] {
//  def prepareFieldLayout = {
//    var requiredLayouts = HashMap[(String, Int), (Datatype, String)]()
//    for (field <- objects)
//      requiredLayouts += ((field.fieldLayoutName, field.level) -> (field.datatype, field.localization))
//
//    def defIndex = Knowledge.dimensionality match {
//      case 1 => l4Dep.Index1D(0)
//      case 2 => l4Dep.Index2D(0, 0)
//      case 3 => l4Dep.Index3D(0, 0, 0)
//    }
//
//    for (layout <- requiredLayouts.toList.sortBy(_._1._2).sortBy(_._1._1)) {
//      l4.FieldLayoutCollection.add(l4.FieldLayout(
//        layout._1._1, // identifier
//        layout._1._2, // level
//        layout._2._1.progress, // datatype
//        layout._2._2, // localization
//        l4.FieldLayout.default_ghostLayers(layout._2._2), // to be determined later
//        false,
//        l4.FieldLayout.default_duplicateLayers(layout._2._2),
//        false,
//        defIndex))
//    }
//  }
//
//  def addInitFieldsFunction = {
//    val initStmts = ListBuffer[Statement]()
//    for (field <- objects)
//      if (field.initial.isDefined)
//        initStmts += AssignmentStatement(FieldAccess(field, SingleLevelSpecification(field.level)), field.initial.get, "=", None)
//    val fct = FunctionStatement("InitFields", None, UnitDatatype, ListBuffer[Variable](), initStmts)
//    StateManager.root_.asInstanceOf[Root].statements += fct
//  }
//
//  def progress = {
//    for (obj <- objects)
//      l4.FieldCollection.add(obj.progress)
//  }
//}
