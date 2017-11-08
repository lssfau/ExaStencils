package exastencils.discretization.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.l1._
import exastencils.field.l1.L1_FieldCollection
import exastencils.grid.l1.L1_Localization
import exastencils.prettyprinting.PpStream

/// L1_FieldDiscretization

object L1_FieldDiscretization {
  def apply(src : String, levels : Option[L1_DeclarationLevelSpecification], mapping : Option[String], localization : String) =
    new L1_FieldDiscretization(src, levels, mapping, L1_Localization.resolve(localization))
}

case class L1_FieldDiscretization(
    var src : String,
    var levels : Option[L1_DeclarationLevelSpecification],
    var mapping : Option[String],
    var localization : L1_Localization) extends L1_DiscretizationHint {

  override def prettyprint(out : PpStream) = {
    out << src
    if (mapping.isDefined) out << " => " << mapping.get
    out << " on " << localization
  }

  override def process() = {
    val fields = {
      if (levels.isDefined) // explicit level definition is given -> extract levels and map to fields
        L1_LevelSpecification.extractLevelListDefEmpty(levels).to[ListBuffer].map(lvl => L1_FieldCollection.getByIdentifier(src, lvl).get)
      else // otherwise collect all fields with the src name
        L1_FieldCollection.getAllByIdentifier(src)
    }

    fields.foreach(l1Field => {
      l1Field.localization = Some(localization)

      val l2Field = l1Field.getProgressedObj()

      if (mapping.isDefined) l2Field.name = mapping.get
    })
  }
}
