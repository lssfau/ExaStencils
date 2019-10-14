package exastencils.discretization.l1

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.l1._
import exastencils.baseExt.l1.L1_UnresolvedAccess
import exastencils.datastructures._
import exastencils.field.l1.L1_FieldCollection
import exastencils.grid.l1._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_FieldDiscretization

object L1_FieldDiscretization {
  def apply(src : String, levels : Option[L1_DeclarationLevelSpecification], mapping : Option[String], localization : String) =
    new L1_FieldDiscretization(src, levels, mapping, L1_Localization.resolve(localization))

  object ReplacePositionAliases extends QuietDefaultStrategy("Local replace of alias accesses with virtual field accesses") {
    var localization : L1_Localization = _
    var level : L1_AccessLevelSpecification = _

    def resolveVF(dim : Int) : String = {
      localization match {
        case L1_AtNode                                  => s"vf_nodePosition_$dim"
        case L1_AtCellCenter                            => s"vf_cellCenter_$dim"
        case L1_AtFaceCenter(stagDim) if stagDim == dim => s"vf_nodePosition_$dim"
        case L1_AtFaceCenter(stagDim) if stagDim != dim => s"vf_cellCenter_$dim"
        case _                                          => Logger.error(s"Unsupported localization ${ localization }")
      }
    }

    val aliasMap : HashMap[String, Int] = HashMap(("x", 0), ("y", 1), ("z", 2))

    this += new Transformation("Search and replace", {
      case access : L1_UnresolvedAccess if aliasMap.contains(access.name.toLowerCase) =>
        access.name = resolveVF(aliasMap(access.name.toLowerCase))
        access.level = Some(level)
        access
    })
  }

  object ReplaceBoundaryPositionAliases extends QuietDefaultStrategy("Local replace of alias accesses with virtual field accesses") {
    var level : L1_AccessLevelSpecification = _

    val aliasMap : HashMap[String, String] = HashMap(("x", "vf_boundaryPosition_0"), ("y", "vf_boundaryPosition_1"), ("z", "vf_boundaryPosition_2"))

    this += new Transformation("Search and replace", {
      case access : L1_UnresolvedAccess if aliasMap.contains(access.name.toLowerCase) =>
        access.name = aliasMap(access.name.toLowerCase)
        access.level = Some(level)
        access
    })
  }

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

      import L1_FieldDiscretization._

      if (l1Field.initial.isDefined) {
        val initWrapped = L1_ExpressionStatement(l1Field.initial.get)
        ReplacePositionAliases.localization = localization
        ReplacePositionAliases.level = L1_SingleLevel(l1Field.level)
        ReplacePositionAliases.applyStandalone(initWrapped)
        l1Field.initial = Some(initWrapped.expression)
      }

      ReplaceBoundaryPositionAliases.level = L1_SingleLevel(l1Field.level)
      ReplaceBoundaryPositionAliases.applyStandalone(l1Field.boundary)

      val l2Field = l1Field.getProgressedObj()

      if (mapping.isDefined) l2Field.name = mapping.get
    })
  }
}
