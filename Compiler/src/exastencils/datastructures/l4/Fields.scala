package exastencils.datastructures.l4

import exastencils.field.l4.L4_FieldLayoutCollection
import exastencils.knowledge
import exastencils.prettyprinting._

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
      val L4_layout = L4_FieldLayoutCollection.getByIdentifier(extLayout, level).get
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
