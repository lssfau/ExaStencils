package exastencils.datastructures.l4

import exastencils.core._
import exastencils.knowledge.Knowledge
import exastencils.knowledge.FieldLayoutPerDim
import exastencils.knowledge.DimArray
import exastencils.knowledge.Field
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.Node

//object LayoutDeclarationStatementBuilder {
//  def apply(name : String, options : List[LayoutOption]) = {
//    var ghostLayers : Option[Index] = None
//    var ghostLayersCommunication : Option[Boolean] = None
//    var duplicate : Option[Index] = None
//    var duplicateCommunicate : Option[Boolean] = None
//    var innerSize : Option[Index] = None
//
//    options.foreach(option => option.name match {
//      case "ghostlayers" =>
//        ghostLayers = Some(option.value)
//        ghostLayersCommunication = option.hasCommunication
//      case "duplicate" =>
//        duplicate = Some(option.value)
//        duplicateCommunicate = option.hasCommunication
//      case "innerSize" => innerSize = Some(option.value)
//    })
//    
//    return LayoutDeclarationStatement(name, ghostLayers, ghostLayersCommunication, duplicate, )
//  }
//}

case class LayoutDeclarationStatement(var name : String,
                                      var ghostLayers : Option[Index] = None,
                                      var ghostLayersCommunication : Option[Boolean] = None,
                                      var duplicate : Option[Index] = None,
                                      var duplicateCommunicate : Option[Boolean] = None,
                                      var innerSize : Option[Index] = None) extends Node {

  def set(options : List[LayoutOption]) : Unit = { options.foreach(set(_)) }

  def set(option : LayoutOption) : Unit = {
    option.name match {
      case "ghostlayers" =>
        ghostLayers = Some(option.value)
        ghostLayersCommunication = option.hasCommunication
      case "duplicate" =>
        duplicate = Some(option.value)
        duplicateCommunicate = option.hasCommunication
      case "innerSize" => innerSize = Some(option.value)
    }
  }
}

case class LayoutOption(var name : String, var value : Index, var hasCommunication : Option[Boolean]) extends Node

case class FieldDeclarationStatement(var name : String,
                                     var datatype : Datatype,
                                     var layout : String,
                                     var boundary : BinaryExpression,
                                     var level : Option[LevelSpecification]) extends SpecialStatement {
  var communicates = true
  var ghostlayers = 0
  var padding = 0
  var slots = 1
  var bcDir0 = false
  def set(t : TempOption) = { // FIXME hack
    t.key match {
      case "communicates" => communicates = t.value.toBoolean
      case "ghostlayers"  => ghostlayers = t.value.toInt
      case "padding"      => padding = t.value.toInt
      case "slots"        => slots = t.value.toInt
      case "bcDir"        => bcDir0 = t.value.toBoolean
      case _              => exastencils.core.Logger.error(s"Unknown option ${t.key} = ${t.value}")
    }
  }

  def progressToIr : Field = {
    val layoutinstance = StateManager.root_.asInstanceOf[Root].getLayoutByIdentifier(layout).get

    val layoutarray = DimArray().map(dim => new FieldLayoutPerDim(
      if (0 == dim) padding else 0,
      ghostlayers,
      1,
      ((Knowledge.domain_fragLengthPerDim(dim) * (1 << level.get.asInstanceOf[SingleLevelSpecification].level)) + 1) - 2 /*dup*/ ,
      1,
      ghostlayers,
      0)) ++
      (Knowledge.dimensionality until 3).toArray.map(dim => new FieldLayoutPerDim(0, 0, 0, 1, 0, 0, 0))

    new Field(
      name,
      0, // FIXME: domain
      name.toLowerCase + "Data_" + level.get.asInstanceOf[SingleLevelSpecification].level, // HACK
      datatype.progressToIr,
      layoutarray,
      layoutinstance.duplicateCommunicate.getOrElse(false), // FIXME put default elsewhere
      layoutinstance.ghostLayersCommunication.getOrElse(false), // FIXME put default elsewhere
      level.get.asInstanceOf[SingleLevelSpecification].level,
      slots,
      layoutinstance.ghostLayers.get.progressToIr, // FIXME
      Some(boundary) // FIXME
      )

  }
}
