package exastencils.waLBerla.l3

import exastencils.base.l3._
import exastencils.boundary.l3.L3_NoBC
import exastencils.domain.l3.L3_Domain
import exastencils.domain.l3.L3_DomainCollection
import exastencils.grid.l3._
import exastencils.prettyprinting.PpStream

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

  var localization : L3_Localization = L3_AtCellCenter

  var domain : L3_Domain = L3_DomainCollection.getByIdentifier("global").get

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
}
