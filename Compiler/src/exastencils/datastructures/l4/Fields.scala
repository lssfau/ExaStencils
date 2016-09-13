package exastencils.datastructures.l4

import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.l4._
import exastencils.core._
import exastencils.datastructures._
import exastencils.knowledge
import exastencils.logger._
import exastencils.prettyprinting._

case class LayoutOption(var name : String, var value : Index, var hasCommunication : Option[Boolean]) extends Node

object LayoutDeclarationStatement {
  def default_ghostComm : Boolean = false
  def default_dupComm : Boolean = false

  def default_ghostLayers(discretization : String) : Index = knowledge.Knowledge.dimensionality match {
    case 2 => Index2D(1, 1)
    case 3 => Index3D(1, 1, 1)
  }

  def default_duplicateLayers(discretization : String) : Index = knowledge.Knowledge.dimensionality match {
    case 2 => discretization match {
      case "node"      => Index2D(1, 1)
      case "cell"      => Index2D(0, 0)
      case "face_x"    => Index2D(1, 0)
      case "face_y"    => Index2D(0, 1)
      case "edge_node" => Index2D(1, 0)
      case "edge_cell" => Index2D(0, 0)
    }
    case 3 => discretization match {
      case "node"      => Index3D(1, 1, 1)
      case "cell"      => Index3D(0, 0, 0)
      case "face_x"    => Index3D(1, 0, 0)
      case "face_y"    => Index3D(0, 1, 0)
      case "face_z"    => Index3D(0, 0, 1)
      case "edge_node" => Index3D(1, 0, 0)
      case "edge_cell" => Index3D(0, 0, 0)
    }
  }

  def default_innerPoints(discretization : String, identifier : Identifier, duplicateLayers : Option[Index]) : Index = {
    val level = identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level
    val L4_duplicateLayers : Index = duplicateLayers.getOrElse(default_duplicateLayers(discretization))

    knowledge.Knowledge.dimensionality match {
      case 2 => discretization match {
        case "node"      => new Index2D(
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(0) * (1 << level)) + 1) - 2 * L4_duplicateLayers(0),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(1) * (1 << level)) + 1) - 2 * L4_duplicateLayers(1))
        case "cell"      => new Index2D(
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(0) * (1 << level)) + 0) - 2 * L4_duplicateLayers(0),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(1) * (1 << level)) + 0) - 2 * L4_duplicateLayers(1))
        case "face_x"    => new Index2D(
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(0) * (1 << level)) + 1) - 2 * L4_duplicateLayers(0),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(1) * (1 << level)) + 0) - 2 * L4_duplicateLayers(1))
        case "face_y"    => new Index2D(
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(0) * (1 << level)) + 0) - 2 * L4_duplicateLayers(0),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(1) * (1 << level)) + 1) - 2 * L4_duplicateLayers(1))
        case "edge_node" => new Index2D(
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(0) * (1 << level)) + 1) - 2 * L4_duplicateLayers(0),
          1)
        case "edge_cell" => new Index2D(
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(0) * (1 << level)) + 0) - 2 * L4_duplicateLayers(0),
          1)
      }
      case 3 => discretization match {
        case "node"      => new Index3D(
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(0) * (1 << level)) + 1) - 2 * L4_duplicateLayers(0),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(1) * (1 << level)) + 1) - 2 * L4_duplicateLayers(1),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(2) * (1 << level)) + 1) - 2 * L4_duplicateLayers(2))
        case "cell"      => new Index3D(
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(0) * (1 << level)) + 0) - 2 * L4_duplicateLayers(0),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(1) * (1 << level)) + 0) - 2 * L4_duplicateLayers(1),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(2) * (1 << level)) + 0) - 2 * L4_duplicateLayers(2))
        case "face_x"    => new Index3D(
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(0) * (1 << level)) + 1) - 2 * L4_duplicateLayers(0),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(1) * (1 << level)) + 0) - 2 * L4_duplicateLayers(1),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(2) * (1 << level)) + 0) - 2 * L4_duplicateLayers(2))
        case "face_y"    => new Index3D(
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(0) * (1 << level)) + 0) - 2 * L4_duplicateLayers(0),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(1) * (1 << level)) + 1) - 2 * L4_duplicateLayers(1),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(2) * (1 << level)) + 0) - 2 * L4_duplicateLayers(2))
        case "face_z"    => new Index3D(
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(0) * (1 << level)) + 0) - 2 * L4_duplicateLayers(0),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(1) * (1 << level)) + 0) - 2 * L4_duplicateLayers(1),
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(2) * (1 << level)) + 1) - 2 * L4_duplicateLayers(2))
        case "edge_node" => new Index3D(
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(0) * (1 << level)) + 1) - 2 * L4_duplicateLayers(0),
          1,
          1)
        case "edge_cell" => new Index3D(
          ((knowledge.Knowledge.domain_fragmentLengthAsVec(0) * (1 << level)) + 0) - 2 * L4_duplicateLayers(0),
          1,
          1)
      }
    }
  }
}

case class LayoutDeclarationStatement(
    override var identifier : Identifier,
    var datatype : L4_Datatype,
    var discretization : String,
    var ghostLayers : Option[Index] = None,
    var ghostLayersCommunication : Option[Boolean] = None,
    var duplicateLayers : Option[Index] = None,
    var duplicateLayersCommunication : Option[Boolean] = None,
    var innerPoints : Option[Index] = None) extends SpecialStatement with HasIdentifier {

  import LayoutDeclarationStatement._

  def set(options : List[LayoutOption]) : Unit = { options.foreach(set(_)) }

  def set(option : LayoutOption) : Unit = {
    option.name match {
      case "ghostLayers" | "ghost"                 =>
        ghostLayers = Some(option.value)
        ghostLayersCommunication = option.hasCommunication
      case "duplicateLayers" | "duplicate" | "dup" =>
        duplicateLayers = Some(option.value)
        duplicateLayersCommunication = option.hasCommunication
      case "innerPoints" | "innerLayers" | "inner" => innerPoints = Some(option.value)
    }
  }

  override def prettyprint(out : PpStream) = {
    out << "Layout " << identifier.name << "< " << datatype << " >" << '@' << identifier.asInstanceOf[LeveledIdentifier].level << " {\n"
    if (ghostLayers.isDefined) out << "ghostLayers = " << ghostLayers.get << (if (ghostLayersCommunication.getOrElse(false)) " with communication" else "") << '\n'
    if (duplicateLayers.isDefined) out << "duplicateLayers = " << duplicateLayers.get << (if (duplicateLayersCommunication.getOrElse(false)) " with communication" else "") << '\n'
    if (innerPoints.isDefined) out << "innerPoints = " << innerPoints.get << '\n'
    out << "}\n"
  }

  override def progress : knowledge.FieldLayout = progress("GENERIC")

  def progress(targetFieldName : String) : knowledge.FieldLayout = {
    val level = identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level

    val numDimsGrid = knowledge.Knowledge.dimensionality // TODO: adapt for edge data structures
    val numDimsData = numDimsGrid + datatype.dimensionality

    val L4_ghostLayers : Index = ghostLayers.getOrElse(default_ghostLayers(discretization))
    val L4_duplicateLayers : Index = duplicateLayers.getOrElse(default_duplicateLayers(discretization))

    val L4_innerPoints : Index = innerPoints.getOrElse(default_innerPoints(discretization, identifier, duplicateLayers))

    val L4_ghostComm = ghostLayersCommunication.getOrElse(default_ghostComm)
    val L4_dupComm = duplicateLayersCommunication.getOrElse(default_dupComm)

    var layouts = Array[knowledge.FieldLayoutPerDim]()

    // add layouts for grid dimensions
    layouts ++= (0 until numDimsGrid).map(dim => new knowledge.FieldLayoutPerDim(
      0, // default, only first requires != 0
      L4_ghostLayers(dim),
      L4_duplicateLayers(dim),
      L4_innerPoints(dim),
      L4_duplicateLayers(dim),
      L4_ghostLayers(dim),
      0 // default, only first requires != 0
    ))

    // add layouts for additional dimensions introduced by the datatype
    if (numDimsData > numDimsGrid) {
      layouts ++= datatype.getSizeArray.map(size => new knowledge.FieldLayoutPerDim(0, 0, 0, size, 0, 0, 0))
    }

    // add padding only for innermost dimension if required
    if (knowledge.Knowledge.data_alignFieldPointers) {
      val innerLayout : knowledge.FieldLayoutPerDim = layouts(0)
      innerLayout.numPadLayersLeft = (knowledge.Platform.simd_vectorSize - innerLayout.numGhostLayersLeft % knowledge.Platform.simd_vectorSize) % knowledge.Platform.simd_vectorSize
      val total = innerLayout.numPadLayersLeft + innerLayout.numGhostLayersLeft + innerLayout.numDupLayersLeft + innerLayout.numInnerLayers + innerLayout.numDupLayersRight + innerLayout.numGhostLayersRight
      innerLayout.numPadLayersRight = (knowledge.Platform.simd_vectorSize - total % knowledge.Platform.simd_vectorSize) % knowledge.Platform.simd_vectorSize
    }

    // set/ update total
    for (layout <- layouts) layout.updateTotal

    // determine reference offset
    // TODO: this should work for now but may be adapted in the future
    var refOffset = new ir.MultiIndex(Array.fill(numDimsData)(0))
    for (dim <- 0 until numDimsGrid)
      refOffset(dim) = IR_IntegerConstant(layouts(dim).numPadLayersLeft + layouts(dim).numGhostLayersLeft)

    // adapt discretization identifier for low-dimensional primitives
    val finalDiscretization =
    if (discretization.startsWith("edge_"))
      discretization.drop(5)
    else
      discretization

    knowledge.FieldLayout(
      s"${ identifier.name }_$targetFieldName",
      level,
      datatype.progress,
      finalDiscretization,
      layouts,
      numDimsGrid,
      numDimsData,
      refOffset,
      L4_dupComm,
      L4_ghostComm)
  }
}

case class FieldDeclarationStatement(
    override var identifier : Identifier,
    var domain : String,
    var layout : String,
    var boundary : Option[L4_Expression],
    var slots : Integer,
    var index : Int = 0) extends SpecialStatement with HasIdentifier {

  override def prettyprint(out : PpStream) = {
    out << "Field " << identifier.name << "< " << domain << ", " << layout << ", " << boundary.getOrElse(BasicIdentifier("None")) << " >"
    if (slots > 1) out << '[' << slots << ']'
    out << '@' << identifier.asInstanceOf[LeveledIdentifier].level << '\n'
  }

  override def progress : knowledge.Field = {
    val level = identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level

    val IR_layout = if (knowledge.Knowledge.ir_genSepLayoutsPerField) {
      // layouts must not be shared -> generate a field specific layout
      val L4_layout_opt = StateManager.root.asInstanceOf[l4.Root].fieldLayouts.find(
        l => l.identifier.asInstanceOf[LeveledIdentifier].name == layout
          && l.identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level == level)
      if (L4_layout_opt.isEmpty)
        Logger.warn(s"Trying to access invalid field layout $layout on level $level")
      val L4_layout = L4_layout_opt.get
      val IR_layout = L4_layout.progress(identifier.name)
      knowledge.FieldLayoutCollection.fieldLayouts += IR_layout
      IR_layout
    } else {
      // layouts have already been processed -> find the required one
      knowledge.FieldLayoutCollection.getFieldLayoutByIdentifier(layout, level).get
    }

    new knowledge.Field(
      identifier.name,
      index,
      knowledge.DomainCollection.getDomainByIdentifier(domain).get,
      identifier.name.toLowerCase + "Data_" + level,
      IR_layout,
      level,
      slots,
      if (boundary.isDefined) Some(boundary.get.progress) else None)
  }
}

case class StencilFieldDeclarationStatement(
    override var identifier : Identifier,
    var fieldName : String,
    var stencilName : String) extends ExternalDeclarationStatement with HasIdentifier {

  override def prettyprint(out : PpStream) = { out << "StencilField " << identifier.name << "< " << fieldName << " => " << stencilName << " >@" << identifier.asInstanceOf[LeveledIdentifier].level << '\n' }

  override def progress : knowledge.StencilField = {
    new knowledge.StencilField(identifier.name,
      knowledge.FieldCollection.getFieldByIdentifier(fieldName, identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level).get,
      knowledge.StencilCollection.getStencilByIdentifier(stencilName, identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level).get)
  }
}

case class ExternalFieldDeclarationStatement(
    var extIdentifier : String,
    var correspondingField : FieldAccess,
    var extLayout : String) extends ExternalDeclarationStatement {

  override def prettyprint(out : PpStream) = { out << "external Field " << extIdentifier << " <" << extLayout << "> => " << correspondingField << '\n' }

  override def progress : knowledge.ExternalField = {
    val level = correspondingField.level.asInstanceOf[SingleLevelSpecification].level

    val IR_layout = if (knowledge.Knowledge.ir_genSepLayoutsPerField) {
      // layouts must not be shared -> generate a field specific layout
      val L4_layout = StateManager.root.asInstanceOf[l4.Root].fieldLayouts.find(
        l => l.identifier.asInstanceOf[LeveledIdentifier].name == extLayout
          && l.identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level == level).get
      val IR_layout = L4_layout.progress(extIdentifier)
      knowledge.FieldLayoutCollection.fieldLayouts += IR_layout
      IR_layout
    } else {
      // layouts have already been processed -> find the required one
      knowledge.FieldLayoutCollection.getFieldLayoutByIdentifier(extLayout, level).get
    }

    //val IR_layout = knowledge.FieldLayoutCollection.getFieldLayoutByIdentifier(extLayout, level).get

    new knowledge.ExternalField(extIdentifier, correspondingField.resolveField, IR_layout, level)
  }
}
