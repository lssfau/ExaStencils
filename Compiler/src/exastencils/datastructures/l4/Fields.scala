package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.knowledge
import exastencils.prettyprinting._

case class LayoutOption(var name : String, var value : Index, var hasCommunication : Option[Boolean]) extends Node

case class LayoutDeclarationStatement(
    var identifier : Identifier,
    var datatype : Datatype,
    var ghostLayers : Option[Index] = None,
    var ghostLayersCommunication : Option[Boolean] = None,
    var duplicateLayers : Option[Index] = None,
    var duplicateLayersCommunication : Option[Boolean] = None,
    var innerPoints : Option[Index] = None) extends SpecialStatement with HasIdentifier {

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

  var default_ghostLayers : Index = knowledge.Knowledge.dimensionality match {
    case 2 => new Index2D(1, 1)
    case 3 => new Index3D(1, 1, 1)
  }
  var l4_ghostLayers : Index = default_ghostLayers
  var default_duplicateLayers : Index = knowledge.Knowledge.dimensionality match {
    case 2 => new Index2D(1, 1)
    case 3 => new Index3D(1, 1, 1)
  }
  var l4_duplicateLayers : Index = default_duplicateLayers
  var default_innerPoints : Index = knowledge.Knowledge.dimensionality match {
    // needs to be overwritten later
    case 2 => new Index2D(1, 1)
    case 3 => new Index3D(1, 1, 1)
  }
  var l4_innerPoints : Index = default_innerPoints
  var default_ghostComm : Boolean = false
  var l4_ghostComm : Boolean = default_ghostComm
  var default_dupComm : Boolean = false
  var l4_dupComm : Boolean = default_dupComm

  def prettyprint(out : PpStream) = {
    out << "Layout " << identifier.name << "< " << datatype << " >" << '@' << identifier.asInstanceOf[LeveledIdentifier].level << " {\n"
    if (ghostLayers.isDefined) out << "ghostLayers = " << ghostLayers.get << (if (ghostLayersCommunication.getOrElse(false)) " with communication" else "") << '\n'
    if (duplicateLayers.isDefined) out << "duplicateLayers = " << duplicateLayers.get << (if (duplicateLayersCommunication.getOrElse(false)) " with communication" else "") << '\n'
    if (innerPoints.isDefined) out << "innerPoints = " << innerPoints.get << '\n'
    out << "}\n"
  }

  def progressToIr : knowledge.FieldLayout = {
    val level = identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level

    l4_ghostLayers = ghostLayers.getOrElse(default_ghostLayers)
    l4_duplicateLayers = duplicateLayers.getOrElse(new Index3D(1, 1, 1))

    default_innerPoints = knowledge.Knowledge.dimensionality match {
      case 2 => new Index2D(
        ((knowledge.Knowledge.domain_fragLengthPerDim(0) * (1 << level)) + 1) - 2 * l4_duplicateLayers(0),
        ((knowledge.Knowledge.domain_fragLengthPerDim(1) * (1 << level)) + 1) - 2 * l4_duplicateLayers(1))
      case 3 => new Index3D(
        ((knowledge.Knowledge.domain_fragLengthPerDim(0) * (1 << level)) + 1) - 2 * l4_duplicateLayers(0),
        ((knowledge.Knowledge.domain_fragLengthPerDim(1) * (1 << level)) + 1) - 2 * l4_duplicateLayers(1),
        ((knowledge.Knowledge.domain_fragLengthPerDim(2) * (1 << level)) + 1) - 2 * l4_duplicateLayers(2))
    }
    l4_innerPoints = innerPoints.getOrElse(default_innerPoints)

    l4_ghostComm = ghostLayersCommunication.getOrElse(default_ghostComm)
    l4_dupComm = duplicateLayersCommunication.getOrElse(default_dupComm)

    var layouts : Array[knowledge.FieldLayoutPerDim] =
      knowledge.DimArray().map(dim => new knowledge.FieldLayoutPerDim(
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
    innerLayout.numPadLayersLeft = (knowledge.Knowledge.simd_vectorSize - innerLayout.idxDupLeftBegin % knowledge.Knowledge.simd_vectorSize) % knowledge.Knowledge.simd_vectorSize
    innerLayout.numPadLayersRight = (knowledge.Knowledge.simd_vectorSize - innerLayout.evalTotal % knowledge.Knowledge.simd_vectorSize) % knowledge.Knowledge.simd_vectorSize
    for (layout <- layouts) // update total after potentially changing padding
      layout.total = ir.IntegerConstant(layout.evalTotal)

    // support vector data types
    layouts ++= Array(new knowledge.FieldLayoutPerDim(0, 0, 0, datatype.progressToIr.resolveFlattendSize, 0, 0, 0))

    // determine reference offset
    // TODO: this should work for now but may be adapted in the future
    var refOffset = new ir.MultiIndex
    for (dim <- 0 until knowledge.Knowledge.dimensionality)
      refOffset(dim) = ir.IntegerConstant(layouts(dim).idxDupLeftBegin)
    refOffset(knowledge.Knowledge.dimensionality) = ir.IntegerConstant(0)

    knowledge.FieldLayout(identifier.name, level, datatype.progressToIr, layouts, refOffset, l4_dupComm, l4_ghostComm)
  }
}

case class FieldDeclarationStatement(
    var identifier : Identifier,
    var domain : String,
    var layout : String,
    var boundary : Option[Expression],
    var slots : Integer,
    var index : Int = 0) extends SpecialStatement with HasIdentifier {

  def prettyprint(out : PpStream) = {
    out << "Field " << identifier.name << "< " << domain << ", " << layout << ", " << boundary.getOrElse(BasicIdentifier("None")) << " >"
    if (slots > 1) out << '[' << slots << ']'
    out << '@' << identifier.asInstanceOf[LeveledIdentifier].level << '\n'
  }

  override def progressToIr : knowledge.Field = {
    val level = identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level
    val ir_layout = knowledge.FieldLayoutCollection.getFieldLayoutByIdentifier(layout, level).get

    new knowledge.Field(
      identifier.name,
      index,
      knowledge.DomainCollection.getDomainByIdentifier(domain).get,
      identifier.name.toLowerCase + "Data_" + identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level,
      ir_layout,
      level,
      slots,
      if (boundary.isDefined) Some(boundary.get.progressToIr) else None)
  }
}

case class StencilFieldDeclarationStatement(
    var identifier : Identifier,
    var fieldName : String,
    var stencilName : String) extends ExternalDeclarationStatement with HasIdentifier {

  def prettyprint(out : PpStream) = { out << "StencilField " << identifier.name << "< " << fieldName << " => " << stencilName << " >@" << identifier.asInstanceOf[LeveledIdentifier].level << '\n' }

  def progressToIr : knowledge.StencilField = {
    new knowledge.StencilField(identifier.name,
      knowledge.FieldCollection.getFieldByIdentifier(fieldName, identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level).get,
      knowledge.StencilCollection.getStencilByIdentifier(stencilName, identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level).get)
  }
}

case class ExternalFieldDeclarationStatement(
    var extIdentifier : String,
    var correspondingField : FieldAccess,
    var extLayout : String) extends ExternalDeclarationStatement {

  def prettyprint(out : PpStream) = { out << "external Field " << extIdentifier << " <" << extLayout << "> => " << correspondingField << '\n' }

  override def progressToIr : knowledge.ExternalField = {
    val level = correspondingField.level.asInstanceOf[SingleLevelSpecification].level
    val ir_layout = knowledge.FieldLayoutCollection.getFieldLayoutByIdentifier(extLayout, level).get

    new knowledge.ExternalField(extIdentifier, correspondingField.resolveField, ir_layout, level)
  }
}
