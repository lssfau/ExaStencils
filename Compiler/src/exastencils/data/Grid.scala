package exastencils.data

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._

abstract class Grid {
  def invokeResolve(specialField : SpecialFieldAccess) : Expression
}

object Grid_AxisAlignedVariableWidth extends Grid {
  // helper method to map names of special fields to actual member functions implementing the resolving step
  override def invokeResolve(specialField : SpecialFieldAccess) : Expression = {
    val fctName = specialField.fieldName
    fctName.substring(fctName.length() - 2) match {
      case "_x" => {
        val method = this.getClass().getMethods.find(_.getName == fctName.substring(0, fctName.length - 2)).get
        method.invoke(this, specialField.level, specialField.index, specialField.arrayIndex, 0 : Integer).asInstanceOf[Expression]
      }
      case "_y" => {
        val method = this.getClass().getMethods.find(_.getName == fctName.substring(0, fctName.length - 2)).get
        method.invoke(this, specialField.level, specialField.index, specialField.arrayIndex, 1 : Integer).asInstanceOf[Expression]
      }
      case "_z" => {
        val method = this.getClass().getMethods.find(_.getName == fctName.substring(0, fctName.length - 2)).get
        method.invoke(this, specialField.level, specialField.index, specialField.arrayIndex, 2 : Integer).asInstanceOf[Expression]
      }
      case _ => {
        val method = this.getClass().getMethods.find(_.getName == fctName).get
        method.invoke(this, specialField.level, specialField.index, specialField.arrayIndex).asInstanceOf[Expression]
      }
    }
  }

  def projectIdx(baseIndex : MultiIndex, dim : Int) = {
    new MultiIndex(baseIndex(dim), 0, 0, 0)
  }

  // direct accesses
  def get_node_pos(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = FieldCollection.getFieldByIdentifierLevExp(s"node_pos_${dimToString(dim)}", level).get
    FieldAccess(FieldSelection(field, field.level, 0, arrayIndex), projectIdx(index, dim))
  }

  def get_stag_cv_width(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = FieldCollection.getFieldByIdentifierLevExp(s"stag_cv_width_${dimToString(dim)}", level).get
    FieldAccess(FieldSelection(field, field.level, 0, arrayIndex), projectIdx(index, dim))
  }

  // compound accesses
  def get_cell_width(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    var offset = MultiIndex(0, 0, 0, 0)
    offset(dim) = 1
    get_node_pos(level, Duplicate(index) + offset, arrayIndex, dim) - get_node_pos(level, Duplicate(index), arrayIndex, dim)
  }

  def get_cell_center_to_face(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    0.5 * get_cell_width(level, index, arrayIndex, dim)
  }
}

object ResolveSpecialFields extends DefaultStrategy("ResolveSpecialFields") {
  // helper method to branch grid types
  def getGridObject : Grid = {
    val gridType = "AxisAlignedVariableWidth" // TODO: move to knowledge
    gridType match {
      case "AxisAlignedVariableWidth" => Grid_AxisAlignedVariableWidth
    }
  }

  this += new Transformation("SearchAndReplace", {
    case specialField : SpecialFieldAccess => getGridObject.invokeResolve(specialField)
  })
}
