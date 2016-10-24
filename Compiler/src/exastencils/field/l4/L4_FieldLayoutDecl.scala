package exastencils.field.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_LeveledKnowledgeDecl
import exastencils.prettyprinting._

/// L4_FieldLayoutOption

case class L4_FieldLayoutOption(
    var name : String,
    var value : L4_ConstIndex,
    var hasCommunication : Boolean) extends L4_Node with PrettyPrintable {

  override def prettyprint(out : PpStream) = {
    out << name << " = " << value
    if (hasCommunication)
      out << " with communication"
  }
}

/// L4_FieldLayoutDecl

object L4_FieldLayoutDecl {
  def apply(identifier : L4_Identifier, datatype : L4_Datatype, discretization : String, options : List[L4_FieldLayoutOption]) =
    new L4_FieldLayoutDecl(identifier, datatype, discretization, options.to[ListBuffer])
}

case class L4_FieldLayoutDecl(
    override var identifier : L4_Identifier,
    var datatype : L4_Datatype,
    var discretization : String,
    var options : ListBuffer[L4_FieldLayoutOption]) extends L4_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) : Unit = {
    out << "Layout " << identifier.name << "< " << datatype << ", " << discretization << " >" << '@' << identifier.asInstanceOf[L4_LeveledIdentifier].level << " {\n"
    out <<< (options, "\n")
    out << "\n}"
  }

  def evalFieldLayoutValue(optionName : String) : L4_ConstIndex = {
    val option = options.find(_.name == optionName)
    if (option.isDefined)
      option.get.value
    else
      L4_FieldLayout.getDefaultValue(optionName, discretization)
  }

  def evalFieldLayoutBoolean(optionName : String) : Boolean = {
    val option = options.find(_.name == optionName)
    if (option.isDefined)
      option.get.hasCommunication
    else
      L4_FieldLayout.getDefaultBoolean(optionName, discretization)
  }

  def composeLayout(level : Int) : L4_FieldLayout = {
    val numDimsGrid = Knowledge.dimensionality // TODO: adapt for edge data structures

    val numGhost = evalFieldLayoutValue("ghostLayers")
    val numDup = evalFieldLayoutValue("duplicateLayers")

    // determine number of inner points
    val providedInnerPoints = options.find(_.name == "innerPoints")
    val innerPoints : L4_ConstIndex =
      if (providedInnerPoints.isDefined) {
        // user specified values are available -> use those
        providedInnerPoints.get.value
      } else {
        // attempt automatic deduction - TODO: adapt for edge data structures
        discretization match {
          case "node"      => L4_ConstIndex((0 until numDimsGrid).map(dim => ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 1) - 2 * numDup(dim)).toArray)
          case "cell"      => L4_ConstIndex((0 until numDimsGrid).map(dim => ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 0) - 2 * numDup(dim)).toArray)
          case "face_x"    => L4_ConstIndex((0 until numDimsGrid).map(dim =>
            if (0 == dim)
              ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 1) - 2 * numDup(dim)
            else
              ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 0) - 2 * numDup(dim)).toArray)
          case "face_y"    => L4_ConstIndex((0 until numDimsGrid).map(dim =>
            if (1 == dim)
              ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 1) - 2 * numDup(dim)
            else
              ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 0) - 2 * numDup(dim)).toArray)
          case "face_z"    => L4_ConstIndex((0 until numDimsGrid).map(dim =>
            if (2 == dim)
              ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 1) - 2 * numDup(dim)
            else
              ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 0) - 2 * numDup(dim)).toArray)
          case "edge_node" => L4_ConstIndex((0 until numDimsGrid).map(dim =>
            if (0 == dim)
              ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 1) - 2 * numDup(dim)
            else
              0).toArray)
          case "edge_cell" => L4_ConstIndex((0 until numDimsGrid).map(dim =>
            if (0 == dim)
              ((Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + 0) - 2 * numDup(dim)
            else
              0).toArray)
        }
      }

    // compile final layout
    L4_FieldLayout(
      identifier.name, level, numDimsGrid,
      datatype, discretization,
      numGhost,
      evalFieldLayoutBoolean("ghostLayers"),
      numDup,
      evalFieldLayoutBoolean("duplicateLayers"),
      innerPoints)
  }

  override def addToKnowledge() : Unit = {
    identifier match {
      case L4_BasicIdentifier(name)                          =>
        for (level <- Knowledge.levels)
          L4_FieldLayoutCollection.add(composeLayout(level))
      case L4_LeveledIdentifier(name, L4_SingleLevel(level)) =>
        L4_FieldLayoutCollection.add(composeLayout(level))
    }
  }
}

/// L4_ProcessFieldLayoutDeclarations

object L4_ProcessFieldLayoutDeclarations extends DefaultStrategy("Integrating L4 field layout declarations with knowledge") {
  this += Transformation("Process new field layouts", {
    case fieldLayoutDecl : L4_FieldLayoutDecl =>
      fieldLayoutDecl.addToKnowledge()
      None // consume declaration statement
  })
}
