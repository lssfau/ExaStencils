package exastencils.field.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_FieldCollection extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/field/|LAYER_LC|/|LAYER_UC|_FieldCollection.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.field.|LAYER_LC|"""
    printer <<< """"""
    if (L3 == layer) {
      printer <<< """import scala.collection.mutable._"""
      printer <<< """"""
      printer <<< """import exastencils.base.ExaRootNode"""
      printer <<< """import exastencils.base.|LAYER_LC|._"""
      printer <<< """import exastencils.base.|NEXT_LC|.|NEXT_UC|_ConstIndex"""
      printer <<< """import exastencils.config.Knowledge"""
    }
    printer <<< """import exastencils.field.|NEXT_LC|._"""
    printer <<< """import exastencils.knowledge.|LAYER_LC|.|LAYER_UC|_KnowledgeContainer._"""
    printer <<< """import exastencils.knowledge.|LAYER_LC|._"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_FieldCollection"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_FieldCollection extends |LAYER_UC|_LeveledKnowledgeCollection[|LAYER_UC|_Field, |NEXT_UC|_Field] {"""
    printer <<< """  exastencils.core.Duplicate.registerConstant(this)"""
    printer <<< """"""
    printer <<< """  |LAYER_UC|_KnowledgeContainer.register(this)"""
    printer <<< """"""
    printer <<< """  |LAYER_UC|_PrepareDeclarations.strategies += |LAYER_UC|_PrepareFieldDeclarations"""
    printer <<< """  |LAYER_UC|_ProcessDeclarations.strategies += |LAYER_UC|_ProcessFieldDeclarations"""
    printer <<< """"""
    printer <<< """  |LAYER_UC|_PrepareAccesses.strategies += |LAYER_UC|_PrepareFieldAccesses"""
    printer <<< """  |LAYER_UC|_ResolveAccesses.strategies += |LAYER_UC|_ResolveFieldAccesses"""
    printer <<< """"""
    printer <<< """  override def name = "|LAYER_UC|_FieldCollection""""
    if (L3 == layer) {
      printer <<< """  override def progress() = {"""
    }
    if (L2 == layer) {
      printer <<< """  override def progress() = objects.foreach(obj => |NEXT_UC|_FieldCollection.add(obj.progress()))"""
    }
    if (L3 == layer) {
      printer <<< """    prepareFieldLayouts()"""
      printer <<< """    objects.foreach(obj => |NEXT_UC|_FieldCollection.add(obj.progress()))"""
      printer <<< """  }"""
      printer <<< """"""
      printer <<< """  def prepareFieldLayouts() = {"""
      printer <<< """    var requiredLayouts = HashMap[(String, Int), (|LAYER_UC|_Datatype, String)]()"""
      printer <<< """    for (field <- objects)"""
      printer <<< """      requiredLayouts += ((field.fieldLayoutName, field.level) -> (field.datatype, field.localization))"""
      printer <<< """"""
      printer <<< """    def defIndex = |NEXT_UC|_ConstIndex(Array.fill(Knowledge.dimensionality - 1)(0))"""
      printer <<< """"""
      printer <<< """    for (layout <- requiredLayouts.toList.sortBy(_._1._2).sortBy(_._1._1)) {"""
      printer <<< """      // generate layout"""
      printer <<< """      val genLayout = |NEXT_UC|_FieldLayout("""
      printer <<< """        layout._1._1, // name"""
      printer <<< """        layout._1._2, // level"""
      printer <<< """        1, // FIXME: numDimsGrid"""
      printer <<< """        layout._2._1.progress, // datatype"""
      printer <<< """        layout._2._2, // localization"""
      printer <<< """        |NEXT_UC|_FieldLayout.default_ghostLayers(layout._2._2), // to be determined later"""
      printer <<< """        false,"""
      printer <<< """        |NEXT_UC|_FieldLayout.default_duplicateLayers(layout._2._2),"""
      printer <<< """        false,"""
      printer <<< """        defIndex)"""
      printer <<< """"""
      printer <<< """      // register layout"""
      printer <<< """      |NEXT_UC|_FieldLayoutCollection.add(genLayout)"""
      printer <<< """    }"""
      printer <<< """  }"""
      printer <<< """"""
      printer <<< """  def addInitFieldsFunction() = {"""
      printer <<< """    val initStmts = ListBuffer[|LAYER_UC|_Statement]()"""
      printer <<< """    for (field <- objects)"""
      printer <<< """      if (field.initial.isDefined)"""
      printer <<< """        initStmts += |LAYER_UC|_Assignment(|LAYER_UC|_FieldAccess(field), field.initial.get, "=", None)"""
      printer <<< """    val fct = |LAYER_UC|_Function("InitFields", None, |LAYER_UC|_UnitDatatype, initStmts)"""
      printer <<< """    ExaRootNode.|LAYER_LC|_root.nodes += fct"""
      printer <<< """  }"""
    }
    printer <<< """}"""
    printer.toString
  }
}
