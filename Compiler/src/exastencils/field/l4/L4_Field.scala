package exastencils.field.l4

import exastencils.boundary.l4.L4_BoundaryCondition
import exastencils.domain.ir.IR_DomainCollection
import exastencils.field.ir.IR_Field
import exastencils.knowledge.l4.L4_KnowledgeObjectWithLevel
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_Field

object L4_Field {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L4_Field(
    var name : String, // will be used to find the field
    var level : Int, // the level the field lives on
    var index : Int,
    var domainName : String, // FIXME: var domain : L4_Domain
    var fieldLayout : L4_FieldLayout,
    var numSlots : Int,
    var boundary : L4_BoundaryCondition) extends L4_KnowledgeObjectWithLevel {

  override def prettyprintDecl(out : PpStream) = {
    out << "Field " << name
    out << "< " << domainName << ", " << fieldLayout << ", " << boundary << " >"
    if (numSlots > 1) out << "[" << numSlots << "]"
    out << "@" << level << "\n"
  }

  override def progress : IR_Field = {
    progressed = Some(IR_Field(
      name,
      level,
      index,
      IR_DomainCollection.getByIdentifier(domainName).get,
      name.toLowerCase + "Data_" + level,
      fieldLayout.getProgressedObject,
      numSlots,
      boundary.progress))

    progressed.get
  }

  var progressed : Option[IR_Field] = None
  override def getProgressedObject = {
    if (progressed.isEmpty)
      Logger.warn(s"Trying to access invalid progressed object of type ${ this.getClass.getName } with name ${ name }")
    progressed.get
  }
}
