package exastencils.field.l4

import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.l4._
import exastencils.config._
import exastencils.field.ir._
import exastencils.knowledge.l4.L4_KnowledgeObjectWithLevel
import exastencils.prettyprinting._

object L4_FieldLayout {
  // TODO: dimensionality-independent representation, eg L4_ConstIndex(Array.fill(Knowledge.dimensionality)(0))

  def default_ghostLayers(discretization : String) : L4_ConstIndex = Knowledge.dimensionality match {
    case 2 => L4_ConstIndex(0, 0)
    case 3 => L4_ConstIndex(0, 0, 0)
  }

  def default_duplicateLayers(discretization : String) : L4_ConstIndex = Knowledge.dimensionality match {
    case 2 => discretization.toLowerCase match {
      case "node"      => L4_ConstIndex(1, 1)
      case "cell"      => L4_ConstIndex(0, 0)
      case "face_x"    => L4_ConstIndex(1, 0)
      case "face_y"    => L4_ConstIndex(0, 1)
      case "edge_node" => L4_ConstIndex(1, 0)
      case "edge_cell" => L4_ConstIndex(0, 0)
    }
    case 3 => discretization.toLowerCase match {
      case "node"      => L4_ConstIndex(1, 1, 1)
      case "cell"      => L4_ConstIndex(0, 0, 0)
      case "face_x"    => L4_ConstIndex(1, 0, 0)
      case "face_y"    => L4_ConstIndex(0, 1, 0)
      case "face_z"    => L4_ConstIndex(0, 0, 1)
      case "edge_node" => L4_ConstIndex(1, 0, 0)
      case "edge_cell" => L4_ConstIndex(0, 0, 0)
    }
  }

  def getDefaultValue(optionName : String, discretization : String) : L4_ConstIndex = {
    optionName match {
      case "ghostLayers"     => default_ghostLayers(discretization)
      case "duplicateLayers" => default_duplicateLayers(discretization)
    }
  }

  def getDefaultBoolean(optionName : String, discretization : String) : Boolean = {
    optionName match {
      case "ghostLayers"     => false // no communication by default
      case "duplicateLayers" => false // no communication by default
    }
  }
}

case class L4_FieldLayout(
    var name : String, // will be used to find the layout
    var level : Int, // the level the field lives on
    var numDimsGrid : Int, // the number of dimensions of the grid
    var datatype : L4_Datatype,
    var discretization : String,
    var ghostLayers : L4_ConstIndex,
    var communicatesGhosts : Boolean,
    var duplicateLayers : L4_ConstIndex,
    var communicatesDuplicated : Boolean,
    var innerPoints : L4_ConstIndex) extends L4_KnowledgeObjectWithLevel[IR_FieldLayout] {

  override def prettyprintDecl(out : PpStream) = {
    out << "Layout " << name << "< "
    out << datatype << ", "
    out << discretization
    out << " >" << "@(" << level << ") {\n"
    //FIXME: out << "innerPoints = " << innerPoints <<  "\n"
    out << "duplicateLayers = " << duplicateLayers << (if (communicatesDuplicated) " with communication\n" else "\n")
    out << "ghostLayers = " << ghostLayers << (if (communicatesGhosts) " with communication\n" else "\n")
    out << "}\n"
  }

  override def progressImpl() : IR_FieldLayout = {
    // determine full data dimensionality
    val numDimsData = numDimsGrid + datatype.dimensionality

    var layouts = Array[IR_FieldLayoutPerDim]()

    // add layouts for grid dimensions - assume 0 padding as default
    layouts ++= (0 until numDimsGrid).map(dim =>
      IR_FieldLayoutPerDim(0, ghostLayers(dim), duplicateLayers(dim), innerPoints(dim), duplicateLayers(dim), ghostLayers(dim), 0))

    // add layouts for additional dimensions introduced by the datatype - no ghost, dup, pad layers required
    if (numDimsData > numDimsGrid)
      layouts ++= datatype.getSizeArray.map(size => IR_FieldLayoutPerDim(0, 0, 0, size, 0, 0, 0))

    // adapt discretization identifier for low-dimensional primitives - TODO: support edges directly?
    val finalDiscretization =
    if (discretization.startsWith("edge_"))
      discretization.drop(5)
    else
      discretization

    // will be updated afterwards
    val dummyRefOffset = IR_ExpressionIndex(Array.fill(numDimsData)(0))

    val ret = IR_FieldLayout(name, level, datatype.progress, finalDiscretization, layouts, numDimsGrid, numDimsData, dummyRefOffset,
      communicatesDuplicated, communicatesGhosts)

    // update reference offset
    ret.updateDefReferenceOffset()

    ret
  }
}
