package exastencils.waLBerla.l2

import exastencils.base.l2._
import exastencils.boundary.l2.L2_BoundaryCondition
import exastencils.core.Duplicate
import exastencils.domain.l2._
import exastencils.field.l2.L2_Field
import exastencils.fieldlike.l2.L2_FieldLike
import exastencils.grid.l2._
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.l3.L3_WaLBerlaField

/// L2_WaLBerlaField

case class L2_WaLBerlaField(
    var name : String,
    var level : Int,
    var datatype : L2_Datatype,
    var numSlots : Int,
    var initial : Option[L2_Expression],
    var boundary : L2_BoundaryCondition
) extends L2_FieldLike[L3_WaLBerlaField] {

  var localization : L2_Localization = L2_AtCellCenter

  var domain : L2_Domain = L2_DomainCollection.getByIdentifier("global").get

  override def createDuplicate() : L2_WaLBerlaField = {
    L2_WaLBerlaField(name, level, Duplicate(datatype), numSlots, Duplicate(initial), Duplicate(boundary))
  }

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "waLBerla " << L2_Field(name, level, domain, datatype, localization, numSlots, initial, boundary).prettyprintDecl(out)
  }

  override def progressImpl() : L3_WaLBerlaField = {
    L3_WaLBerlaField(
      name,
      level,
      datatype.progress,
      numSlots,
      if (initial.isDefined) Some(initial.get.progress) else None,
      boundary.progress)
  }
  
  override def toField : L2_Field = L2_Field(name, level, domain, datatype, localization, numSlots, initial, boundary)
}
