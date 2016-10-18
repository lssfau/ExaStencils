package exastencils.field.l4

import exastencils.base.l4._
import exastencils.boundary.l4.L4_BoundaryCondition
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_LeveledKnowledgeDecl
import exastencils.prettyprinting.PpStream

/// L4_FieldDecl

object L4_FieldDecl {
  var runningIndex = 0
}

case class L4_FieldDecl(
    override var identifier : L4_Identifier,
    var domainName : String,
    var fieldLayoutName : String,
    var boundary : L4_BoundaryCondition,
    var numSlots : Integer,
    var index : Int = 0) extends L4_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "Field " << identifier.name << "< " << domainName << ", " << fieldLayoutName << ", " << boundary << " >"
    if (numSlots > 1) out << '[' << numSlots << ']'
    out << '@' << identifier.asInstanceOf[L4_LeveledIdentifier].level << '\n'
  }

  def composeField(level : Int) : L4_Field = {
    import L4_FieldDecl.runningIndex

    def index = runningIndex
    runningIndex += 1

    val resolvedFieldLayout = L4_FieldLayoutCollection.getByIdentifier(fieldLayoutName, level).get

    // compile final layout
    L4_Field(identifier.name, level, index, domainName, resolvedFieldLayout, numSlots, boundary)
  }

  override def addToKnowledge() = {
    identifier match {
      case L4_BasicIdentifier(name)                          =>
        for (level <- Knowledge.levels)
          L4_FieldCollection.add(composeField(level))
      case L4_LeveledIdentifier(name, L4_SingleLevel(level)) =>
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
      None // consume declaration statement
  })
}
