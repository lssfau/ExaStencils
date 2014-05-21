package exastencils.datastructures.l4

import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir.ImplicitConversions._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

case class LayoutOption(var name : String, var value : Index, var hasCommunication : Option[Boolean]) extends Node

case class LayoutDeclarationStatement(var name : String,
                                      var ghostLayers : Option[Index] = None,
                                      var ghostLayersCommunication : Option[Boolean] = None,
                                      var duplicateLayers : Option[Index] = None,
                                      var duplicateLayersCommunication : Option[Boolean] = None,
                                      var innerPoints : Option[Index] = None) extends Node {

  def set(options : List[LayoutOption]) : Unit = { options.foreach(set(_)) }

  def set(option : LayoutOption) : Unit = {
    option.name match {
      case "ghostLayers" =>
        ghostLayers = Some(option.value)
        ghostLayersCommunication = option.hasCommunication
      case "duplicateLayers" =>
        duplicateLayers = Some(option.value)
        duplicateLayersCommunication = option.hasCommunication
      case "innerPoints" => innerPoints = Some(option.value)
    }
  }
}

case class FieldDeclarationStatement(var name : String,
                                     var datatype : Datatype,
                                     var layout : String,
                                     var boundary : Option[Expression],
                                     var level : Option[LevelSpecification]) extends SpecialStatement {

  var slots = 1 // FIXME: add to parser

  def progressToIr : Field = {
    val l4_layout = StateManager.root_.asInstanceOf[Root].getLayoutByIdentifier(layout).get
    val l4_ghostLayers = l4_layout.ghostLayers.getOrElse(new Index3D(1, 1, 1))
    val l4_duplicateLayers = l4_layout.duplicateLayers.getOrElse(new Index3D(1, 1, 1))
    val l4_innerPoints = l4_layout.innerPoints.getOrElse(new Index3D(
      ((Knowledge.domain_fragLengthPerDim(0) * (1 << level.get.asInstanceOf[SingleLevelSpecification].level)) + 1) - 2 * l4_duplicateLayers(0),
      ((Knowledge.domain_fragLengthPerDim(1) * (1 << level.get.asInstanceOf[SingleLevelSpecification].level)) + 1) - 2 * l4_duplicateLayers(1),
      ((Knowledge.domain_fragLengthPerDim(2) * (1 << level.get.asInstanceOf[SingleLevelSpecification].level)) + 1) - 2 * l4_duplicateLayers(2)))
    val l4_ghostComm = l4_layout.ghostLayersCommunication.getOrElse(false)
    val l4_dupComm = l4_layout.duplicateLayersCommunication.getOrElse(false)

    val ir_layout = DimArray().map(dim => new FieldLayoutPerDim(
      if (0 == dim) 0 else 0, // FIXME: add padding
      l4_ghostLayers(dim),
      l4_duplicateLayers(dim),
      l4_innerPoints(dim),
      l4_duplicateLayers(dim),
      l4_ghostLayers(dim),
      0 // FIXME: check if padding at the end will be required
      )) ++
      (Knowledge.dimensionality until 3).toArray.map(dim => new FieldLayoutPerDim(0, 0, 0, 1, 0, 0, 0))

    new Field(
      name,
      0, // FIXME: domain
      name.toLowerCase + "Data_" + level.get.asInstanceOf[SingleLevelSpecification].level,
      datatype.progressToIr,
      ir_layout,
      l4_dupComm,
      l4_ghostComm,
      level.get.asInstanceOf[SingleLevelSpecification].level,
      slots,
      l4_ghostLayers.progressToIr, // TODO: this should work for now but may be adapted in the future
      if (boundary.isDefined) Some(boundary.get.progressToIr) else None)
  }
}

case class ExternalFieldDeclarationStatement(
    var externalidentifier : String,
    var internalidentifier : Identifier,
    var layout : String) extends ExternalDeclarationStatement {
  def progressToIr = { throw new NotImplementedException }
}

