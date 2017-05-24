package exastencils.field.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_BoundaryFieldDecl extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/field/|LAYER_LC|/|LAYER_UC|_BoundaryFieldDecl.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.field.|LAYER_LC|

import exastencils.base.|LAYER_LC|._
import exastencils.boundary.|LAYER_LC|._
import exastencils.datastructures._
import exastencils.knowledge.|LAYER_LC|._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// |LAYER_UC|_BoundaryFieldDecl

case class |LAYER_UC|_BoundaryFieldDecl(
    var name : String,
    var levels : Option[|LAYER_UC|_LevelSpecification],
    var boundary : |LAYER_UC|_BoundaryCondition) extends |LAYER_UC|_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"
  override def progress = Logger.error(s"Trying to progress |LAYER_UC| boundary declaration for field $name; this is not supported")

  def addToKnowledge() : Unit = {
    val fieldToAdapt = |LAYER_UC|_FieldCollection.getByIdentifier(name, |LAYER_UC|_LevelSpecification.asSingleLevel(levels)).get
    fieldToAdapt.boundary = boundary
  }
}

/// |LAYER_UC|_ProcessBoundaryDeclarations

object |LAYER_UC|_ProcessBoundaryDeclarations extends DefaultStrategy("Integrate |LAYER_UC| boundary declarations with knowledge") {
  this += Transformation("Adapt bc's of new fields", {
    case decl : |LAYER_UC|_BoundaryFieldDecl if !|LAYER_UC|_FutureKnowledgeAccess.existsInStmt(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
"""
  }
}
