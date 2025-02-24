package exastencils.waLBerla.l2.field

import exastencils.base.l2._
import exastencils.boundary.l2.L2_NoBC
import exastencils.fieldlike.l2.L2_FieldLikeCollection
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.l3.field.L3_WaLBerlaField

/// L2_WaLBerlaBaseFieldDecl

object L2_WaLBerlaBaseFieldDecl {
  def apply(identifier : String, levels : Option[L2_LevelSpecification], datatype : Option[L2_Datatype], numSlots : Option[Int], initial : Option[L2_Expression]) : L2_WaLBerlaBaseFieldDecl =
    L2_WaLBerlaBaseFieldDecl(identifier, levels, datatype.getOrElse(L2_RealDatatype), numSlots, initial)
}

case class L2_WaLBerlaBaseFieldDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var datatype : L2_Datatype,
    var numSlots : Option[Int],
    var initial : Option[L2_Expression]
) extends L2_WaLBerlaFieldDecl {

  override def prettyprint(out : PpStream) = {
    out << "waLBerla Field " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " with " << datatype
    if (numSlots.isDefined) out << " " << numSlots.get << " times"
    if (initial.isDefined) out << " = " << initial.get
  }

  override def addToKnowledge() : Unit = {
    L2_WaLBerlaFieldCollection.add(
      L2_WaLBerlaField(
        name,
        L2_LevelSpecification.asSingleLevel(levels),
        datatype,
        numSlots.getOrElse(1),
        initial,
        L2_NoBC))
  }

  override def associatedCollection : L2_FieldLikeCollection[L2_WaLBerlaField, L3_WaLBerlaField] = L2_WaLBerlaFieldCollection
}
