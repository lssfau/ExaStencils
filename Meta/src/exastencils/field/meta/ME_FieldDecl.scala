package exastencils.field.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_FieldDecl extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/field/|LAYER_LC|/|LAYER_UC|_FieldDecl.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.field.|LAYER_LC|"""
    printer <<< """"""
    printer <<< """import exastencils.datastructures._"""
    printer <<< """import exastencils.knowledge.|LAYER_LC|._"""
    printer <<< """import exastencils.logger._"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_FieldDecl"""
    printer <<< """"""
    printer <<< """abstract class |LAYER_UC|_FieldDecl extends |LAYER_UC|_LeveledKnowledgeDecl {"""
    if (L2 == layer) {
      printer <<< """  override def progress = Logger.error(s"Trying to progress |LAYER_UC| field declaration for field $name; this is not supported")"""
    }
    if (L3 == layer) {
      printer <<< """  override def progress = Logger.error(s"Trying to progress |LAYER_LC| field declaration for field $name; this is not supported")"""
    }
    printer <<< """  def addToKnowledge() : Unit"""
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_PrepareFieldDeclaration"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_PrepareFieldDeclarations extends DefaultStrategy("Prepare knowledge for |LAYER_UC| fields") {"""
    printer <<< """  this += Transformation("Process new fields", {"""
    printer <<< """    case decl : |LAYER_UC|_FieldDecl =>"""
    printer <<< """      |LAYER_UC|_FieldCollection.addDeclared(decl.name, decl.levels)"""
    printer <<< """      decl // preserve declaration statement"""
    printer <<< """  })"""
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_ProcessFieldDeclarations"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_ProcessFieldDeclarations extends DefaultStrategy("Integrate |LAYER_UC| field declarations with knowledge") {"""
    printer <<< """  this += Transformation("Process field declarations", {"""
    printer <<< """    case decl : |LAYER_UC|_FieldDecl if !|LAYER_UC|_FutureKnowledgeAccess.existsInStmt(decl) =>"""
    printer <<< """      decl.addToKnowledge()"""
    printer <<< """      None // consume declaration statement"""
    printer <<< """  })"""
    printer <<< """}"""
    printer.toString
  }
}
