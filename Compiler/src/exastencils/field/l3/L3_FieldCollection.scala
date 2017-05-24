package exastencils.field.l3

import scala.collection.mutable._

import exastencils.base.ExaRootNode
import exastencils.base.l3._
import exastencils.base.l4.L4_ConstIndex
import exastencils.config.Knowledge
import exastencils.field.l4._
import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3._

/// L3_FieldCollection

object L3_FieldCollection extends L3_LeveledKnowledgeCollection[L3_Field, L4_Field] {
  exastencils.core.Duplicate.registerConstant(this)

  L3_KnowledgeContainer.register(this)

  L3_PrepareDeclarations.strategies += L3_PrepareFieldDeclarations
  L3_ProcessDeclarations.strategies += L3_ProcessFieldDeclarations

  L3_PrepareAccesses.strategies += L3_PrepareFieldAccesses
  L3_ResolveAccesses.strategies += L3_ResolveFieldAccesses

  override def name = "L3_FieldCollection"
  override def progress() = {
    prepareFieldLayouts()
    objects.foreach(obj => L4_FieldCollection.add(obj.progress()))
  }

  def prepareFieldLayouts() = {
    var requiredLayouts = HashMap[(String, Int), (L3_Datatype, String)]()
    for (field <- objects)
      requiredLayouts += ((field.fieldLayoutName, field.level) -> (field.datatype, field.localization))

    def defIndex = L4_ConstIndex(Array.fill(Knowledge.dimensionality - 1)(0))

    for (layout <- requiredLayouts.toList.sortBy(_._1._2).sortBy(_._1._1)) {
      // generate layout
      val genLayout = L4_FieldLayout(
        layout._1._1, // name
        layout._1._2, // level
        1, // FIXME: numDimsGrid
        layout._2._1.progress, // datatype
        layout._2._2, // localization
        L4_FieldLayout.default_ghostLayers(layout._2._2), // to be determined later
        false,
        L4_FieldLayout.default_duplicateLayers(layout._2._2),
        false,
        defIndex)

      // register layout
      L4_FieldLayoutCollection.add(genLayout)
    }
  }

  def addInitFieldsFunction() = {
    val initStmts = ListBuffer[L3_Statement]()
    for (field <- objects)
      if (field.initial.isDefined)
        initStmts += L3_Assignment(L3_FieldAccess(field), field.initial.get, "=", None)
    val fct = L3_Function("InitFields", None, L3_UnitDatatype, initStmts)
    ExaRootNode.l3_root.nodes += fct
  }
}
