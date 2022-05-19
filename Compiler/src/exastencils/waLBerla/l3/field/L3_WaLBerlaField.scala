package exastencils.waLBerla.l3.field

import exastencils.base.l3.L3_Datatype
import exastencils.base.l3.L3_Expression
import exastencils.boundary.l3.L3_BoundaryCondition
import exastencils.core.Duplicate
import exastencils.domain.l3.L3_Domain
import exastencils.domain.l3.L3_DomainCollection
import exastencils.field.l3.L3_Field
import exastencils.fieldlike.l3.L3_FieldLike
import exastencils.grid.l3.L3_AtCellCenter
import exastencils.grid.l3.L3_Localization
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.l4.field.L4_WaLBerlaField
import exastencils.waLBerla.l4.field.L4_WaLBerlaFieldLayoutCollection

case class L3_WaLBerlaField(
    var name : String,
    var level : Int,
    var datatype : L3_Datatype,
    var numSlots : Int,
    var initial : Option[L3_Expression],
    var boundary : L3_BoundaryCondition
) extends L3_FieldLike[L4_WaLBerlaField] {

  var localization : L3_Localization = L3_AtCellCenter

  var domain : L3_Domain = L3_DomainCollection.getByIdentifier("global").get

  override def createDuplicate() : L3_WaLBerlaField = {
    L3_WaLBerlaField(name, level, Duplicate(datatype), numSlots, Duplicate(initial), Duplicate(boundary))
  }

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "waLBerla " << L3_Field(name, level, domain, datatype, localization, numSlots, initial, boundary).prettyprintDecl(out)
  }

  override def fieldLayoutName : String = s"defWaLBerlaLayoutFor_${ printDatatype(datatype) }_on_${ localization.prettyprint() }"

  override def progressImpl() : L4_WaLBerlaField = {
    L4_WaLBerlaField(
      name,
      level,
      -1, // index is to be set later
      L4_WaLBerlaFieldLayoutCollection.getByIdentifier(fieldLayoutName, level).get, // l3 field layout is not available -> grab l4 layout directly
      numSlots,
      boundary.progress)
  }

  override def toField : L3_Field = L3_Field(name, level, domain, datatype, localization, numSlots, initial, boundary)
}
