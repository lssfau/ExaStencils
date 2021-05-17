package exastencils.waLBerla.l4

import exastencils.base.l4.L4_DeclarationLevelSpecification
import exastencils.domain.l4.L4_DomainAccess
import exastencils.field.l4.L4_BaseFieldDecl
import exastencils.field.l4.L4_Field
import exastencils.field.l4.L4_FieldDecl
import exastencils.field.l4.L4_FieldLayoutAccess
import exastencils.prettyprinting.PpStream

case class L4_WaLBerlaFieldDecl(decl : L4_BaseFieldDecl) extends L4_FieldDecl {

  override var levels : Option[L4_DeclarationLevelSpecification] = decl.levels
  override def name : String = decl.name

  override def prettyprint(out : PpStream) : Unit = {
    out << "waLBerla " << decl.prettyprint()
  }

  override def addToKnowledge() : Unit = {
    val index = L4_FieldDecl.runningIndex
    L4_FieldDecl.runningIndex += 1

    L4_WaLBerlaFieldCollection.add(
      L4_WaLBerlaField(
        L4_Field(
          decl.name,
          levels.get.resolveLevel,
          index,
          decl.domain.asInstanceOf[L4_DomainAccess].target,
          decl.fieldLayout.asInstanceOf[L4_FieldLayoutAccess].target,
          decl.numSlots,
          decl.boundary,
          decl.matShape
        )))
  }
}