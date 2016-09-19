package exastencils.field.l4

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.knowledge.Knowledge
import exastencils.prettyprinting.PpStream

/// L4_FieldDeclaration

case class L4_FieldDecl(
    override var identifier : Identifier,
    var domain : String,
    var fieldLayout : String,
    var boundary : Option[L4_Expression],
    var numSlots : Integer,
    var index : Int = 0) extends L4_KnowledgeDeclStatement with HasIdentifier {

  override def prettyprint(out : PpStream) = {
    out << "Field " << identifier.name << "< " << domain << ", " << fieldLayout << ", " << boundary.getOrElse(BasicIdentifier("None")) << " >"
    if (numSlots > 1) out << '[' << numSlots << ']'
    out << '@' << identifier.asInstanceOf[LeveledIdentifier].level << '\n'
  }

  def composeField(level : Int) : L4_Field = {
    // get index - TODO: incremental index
    def index = 0

    // compile final layout
    L4_Field(identifier.name, level, index, domain, fieldLayout, numSlots, boundary)
  }

  override def addToKnowledge() = {
    identifier match {
      case BasicIdentifier(name)                                    =>
        for (level <- Knowledge.levels)
          L4_FieldCollection.add(composeField(level))
      case LeveledIdentifier(name, SingleLevelSpecification(level)) =>
        L4_FieldCollection.add(composeField(level))
    }
    None // consume declaration statement
  }
}

/// L4_ProcessFieldDeclarations

object L4_ProcessFieldDeclarations extends DefaultStrategy("Integrating L4 field declarations with knowledge") {
  this += Transformation("Process new fields", {
    case fieldDecl : L4_FieldDecl =>
      fieldDecl.addToKnowledge()
      None
  })
}
