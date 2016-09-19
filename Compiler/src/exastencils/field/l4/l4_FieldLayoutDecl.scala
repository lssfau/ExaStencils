package exastencils.field.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.knowledge.Knowledge
import exastencils.prettyprinting._

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

case class L4_FieldLayoutDecl(
    override var identifier : Identifier,
    var datatype : L4_Datatype,
    var discretization : String,
    var options : ListBuffer[L4_FieldLayoutOption]) extends L4_KnowledgeDeclStatement with HasIdentifier {

  override def prettyprint(out : PpStream) : Unit = {
    out << "Layout " << identifier.name << "< " << datatype << " >" << '@' << identifier.asInstanceOf[LeveledIdentifier].level << " {\n"
    out <<< (options, "\n")
    out << "\n}\n"
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

    val innerPoints : L4_ConstIndex = ???

    val numGhost = evalFieldLayoutValue("ghostLayers")
    val numDup = evalFieldLayoutValue("duplicateLayers")

    // compile final layout
    L4_FieldLayout(
      identifier.name, level,
      datatype, discretization,
      numGhost,
      evalFieldLayoutBoolean("ghostLayers"),
      numDup,
      evalFieldLayoutBoolean("duplicateLayers"),
      innerPoints)
  }

  override def addToKnowledge() = {
    identifier match {
      case BasicIdentifier(name)                                    =>
        for (level <- Knowledge.levels)
          L4_FieldLayoutCollection.add(composeLayout(level))
      case LeveledIdentifier(name, SingleLevelSpecification(level)) =>
        L4_FieldLayoutCollection.add(composeLayout(level))
    }
    None // consume declaration statement
  }
}

/// L4_ProcessFieldLayoutDeclarations

object L4_ProcessFieldLayoutDeclarations extends DefaultStrategy("Integrating L4 field layout declarations with knowledge") {
  this += Transformation("Process new field layouts", {
    case fieldLayoutDecl : L4_FieldLayoutDecl =>
      fieldLayoutDecl.addToKnowledge()
      None
  })
}
