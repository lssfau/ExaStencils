package exastencils.datastructures.l4

import exastencils.core._
import exastencils.knowledge
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.NullExpression
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import exastencils.optimization.Vectorization

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

  var default_ghostLayers : Index = Knowledge.dimensionality match {
    case 2 => new Index2D(1, 1)
    case 3 => new Index3D(1, 1, 1)
  }
  var l4_ghostLayers : Index = default_ghostLayers
  var default_duplicateLayers : Index = Knowledge.dimensionality match {
    case 2 => new Index2D(1, 1)
    case 3 => new Index3D(1, 1, 1)
  }
  var l4_duplicateLayers : Index = default_duplicateLayers
  var default_innerPoints : Index = Knowledge.dimensionality match {
    // needs to be overwritten later
    case 2 => new Index2D(1, 1)
    case 3 => new Index3D(1, 1, 1)
  }
  var l4_innerPoints : Index = default_innerPoints
  var default_ghostComm : Boolean = false
  var l4_ghostComm : Boolean = default_ghostComm
  var default_dupComm : Boolean = false
  var l4_dupComm : Boolean = default_dupComm

  def progressToIr(level : Int) : Array[knowledge.FieldLayoutPerDim] = {
    l4_ghostLayers = ghostLayers.getOrElse(default_ghostLayers)
    l4_duplicateLayers = duplicateLayers.getOrElse(new Index3D(1, 1, 1))

    default_innerPoints = Knowledge.dimensionality match {
      case 2 => new Index2D(
        ((Knowledge.domain_fragLengthPerDim(0) * (1 << level)) + 1) - 2 * l4_duplicateLayers(0),
        ((Knowledge.domain_fragLengthPerDim(1) * (1 << level)) + 1) - 2 * l4_duplicateLayers(1))
      case 3 => new Index3D(
        ((Knowledge.domain_fragLengthPerDim(0) * (1 << level)) + 1) - 2 * l4_duplicateLayers(0),
        ((Knowledge.domain_fragLengthPerDim(1) * (1 << level)) + 1) - 2 * l4_duplicateLayers(1),
        ((Knowledge.domain_fragLengthPerDim(2) * (1 << level)) + 1) - 2 * l4_duplicateLayers(2))
    }
    l4_innerPoints = innerPoints.getOrElse(default_innerPoints)

    l4_ghostComm = ghostLayersCommunication.getOrElse(default_ghostComm)
    l4_dupComm = duplicateLayersCommunication.getOrElse(default_dupComm)

    val layouts : Array[knowledge.FieldLayoutPerDim] =
      DimArray().map(dim => new knowledge.FieldLayoutPerDim(
        0, // default, only first requires != 0
        l4_ghostLayers(dim),
        l4_duplicateLayers(dim),
        l4_innerPoints(dim),
        l4_duplicateLayers(dim),
        l4_ghostLayers(dim),
        0 // default, only first requires != 0
        ))
    // TODO: check if padding works properly (for vectorization)
    // add padding only for innermost dimension
    val innerLayout : knowledge.FieldLayoutPerDim = layouts(0)
    innerLayout.numPadLayersLeft = (Vectorization.TMP_VS - innerLayout.idxDupLeftBegin % Vectorization.TMP_VS) % Vectorization.TMP_VS
    innerLayout.numPadLayersRight = (Vectorization.TMP_VS - innerLayout.total % Vectorization.TMP_VS) % Vectorization.TMP_VS
    return layouts
  }
}

case class FieldDeclarationStatement(var name : String,
    var datatype : Datatype,
    var domain : String,
    var layout : String,
    var boundary : Option[Expression],
    var level : Option[LevelSpecification],
    var slots : Integer,
    var index : Int = 0) extends SpecialStatement {

  override def progressToIr : knowledge.Field = {
    val l4_layout = StateManager.root_.asInstanceOf[Root].getLayoutByIdentifier(layout).get

    var ir_layout = l4_layout.progressToIr(level.get.asInstanceOf[SingleLevelSpecification].level)
    ir_layout ++= Array(new knowledge.FieldLayoutPerDim(0, 0, 0, datatype.progressToIr.resolveFlattendSize, 0, 0, 0))

    var refOffset = new ir.MultiIndex // TODO: this should work for now but may be adapted in the future
    for (dim <- 0 until Knowledge.dimensionality)
      refOffset(dim) = ir_layout(dim).idxDupLeftBegin
    refOffset(Knowledge.dimensionality) = 0

    new knowledge.Field(
      name,
      index,
      DomainCollection.getDomainByIdentifier(domain).get,
      name.toLowerCase + "Data_" + level.get.asInstanceOf[SingleLevelSpecification].level,
      datatype.progressToIr,
      ir_layout,
      l4_layout.l4_dupComm,
      l4_layout.l4_ghostComm,
      level.get.asInstanceOf[SingleLevelSpecification].level,
      slots,
      refOffset,
      if (boundary.isDefined) Some(boundary.get.progressToIr) else None,
      if (Knowledge.data_addPrePadding)
        4 - (ir_layout(0).idxDupLeftBegin + ir.ArrayAccess(new ir.iv.IterationOffsetBegin(domain), 0)) // TODO: specify correct alignmentPadding
      else
        NullExpression())
  }
}

case class StencilFieldDeclarationStatement(var name : String, var fieldName : String, var stencilName : String, var level : Option[LevelSpecification]) extends ExternalDeclarationStatement {
  def progressToIr : knowledge.StencilField = {
    new knowledge.StencilField(name,
      FieldCollection.getFieldByIdentifier(fieldName, level.get.asInstanceOf[SingleLevelSpecification].level).get,
      StencilCollection.getStencilByIdentifier(stencilName, level.get.asInstanceOf[SingleLevelSpecification].level).get)
  }
}

case class ExternalFieldDeclarationStatement(
    var extIdentifier : String,
    var correspondingField : FieldAccess,
    var extLayout : String) extends ExternalDeclarationStatement {

  override def progressToIr : knowledge.ExternalField = {
    val l4_layout = StateManager.root_.asInstanceOf[Root].getLayoutByIdentifier(extLayout).get
    val ir_layout = l4_layout.progressToIr(correspondingField.level.asInstanceOf[SingleLevelSpecification].level)

    new knowledge.ExternalField(extIdentifier,
      correspondingField.resolveField,
      ir_layout,
      correspondingField.level.asInstanceOf[SingleLevelSpecification].level,
      l4_layout.l4_ghostLayers.progressToIr // TODO: this should work for now but may be adapted in the future)
      )
  }
}
