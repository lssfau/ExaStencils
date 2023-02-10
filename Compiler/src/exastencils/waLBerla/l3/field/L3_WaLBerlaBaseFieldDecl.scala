package exastencils.waLBerla.l3.field

import exastencils.base.l3._
import exastencils.boundary.l3.L3_NoBC
import exastencils.fieldlike.l3.L3_FieldLikeCollection
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.l4.field.L4_WaLBerlaField

/// L3_WaLBerlaBaseFieldDecl

object L3_WaLBerlaBaseFieldDecl {
  def apply(identifier : String, levels : Option[L3_LevelSpecification], datatype : Option[L3_Datatype], numSlots : Option[Int], initial : Option[L3_Expression]) : L3_WaLBerlaBaseFieldDecl =
    L3_WaLBerlaBaseFieldDecl(identifier, levels, datatype.getOrElse(L3_RealDatatype), numSlots, initial)
}


case class L3_WaLBerlaBaseFieldDecl(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var datatype : L3_Datatype,
    var numSlots : Option[Int],
    var initial : Option[L3_Expression]
) extends L3_WaLBerlaFieldDecl {

  override def prettyprint(out : PpStream) = {
    out << "waLBerla Field " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " with " << datatype
    if (numSlots.isDefined) out << " " << numSlots.get << " times"
    if (initial.isDefined) out << " = " << initial.get
  }

  override def addToKnowledge() : Unit = {
    L3_WaLBerlaFieldCollection.add(
      L3_WaLBerlaField(
        name,
        L3_LevelSpecification.asSingleLevel(levels),
        datatype,
        numSlots.getOrElse(1),
        initial,
        L3_NoBC))
  }

  override def associatedCollection : L3_FieldLikeCollection[L3_WaLBerlaField, L4_WaLBerlaField] = L3_WaLBerlaFieldCollection
}
