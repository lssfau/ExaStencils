package exastencils.waLBerla.l4

import exastencils.base.l4.L4_Datatype
import exastencils.core.Duplicate
import exastencils.field.l4.L4_Field
import exastencils.grid.l4.L4_Localization
import exastencils.knowledge.l4.L4_LeveledKnowledgeObject
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.IR_WaLBerlaField

case class L4_WaLBerlaField(
    var field : L4_Field,
    var dummyVar : Boolean = false // TODO replace
) extends L4_LeveledKnowledgeObject[IR_WaLBerlaField] {

  override def createDuplicate() : L4_WaLBerlaField = {
    L4_WaLBerlaField.tupled(Duplicate(L4_WaLBerlaField.unapply(this).get))
  }

  def datatype : L4_Datatype = field.fieldLayout.datatype
  def localization : L4_Localization = field.fieldLayout.localization
  override def level : Int = field.level

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "waLBerla " << field.prettyprintDecl(out)
  }

  def name : String = field.name
  def codeName : String = name + "_" + level
  def numDimsGrid : Int = field.domain.numDims

  override def progressImpl() = IR_WaLBerlaField(field.getProgressedObj())
}
