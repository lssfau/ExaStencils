package exastencils.data

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._

abstract class Grid {
  def invokeAccessResolve(specialField : SpecialFieldAccess) : Expression
  def invokeEvalResolve(functionName : String, fieldAccess : FieldAccess) : Expression
}

// helper object/method to branch grid types
object Grid {
  def getGridObject : Grid = {
    val gridType = "AxisAlignedVariableWidth" // TODO: move to knowledge
    gridType match {
      case "AxisAlignedVariableWidth" => Grid_AxisAlignedVariableWidth
    }
  }
}

object Grid_AxisAlignedVariableWidth extends Grid {
  // helper method to map names of special fields to actual member functions implementing the resolving step
  override def invokeAccessResolve(specialField : SpecialFieldAccess) : Expression = {
    val functionName = specialField.fieldName
    functionName.substring(functionName.length() - 2) match {
      case "_x" => {
        val method = this.getClass().getMethods.find(_.getName == functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, specialField.level, specialField.index, specialField.arrayIndex, 0 : Integer).asInstanceOf[Expression]
      }
      case "_y" => {
        val method = this.getClass().getMethods.find(_.getName == functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, specialField.level, specialField.index, specialField.arrayIndex, 1 : Integer).asInstanceOf[Expression]
      }
      case "_z" => {
        val method = this.getClass().getMethods.find(_.getName == functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, specialField.level, specialField.index, specialField.arrayIndex, 2 : Integer).asInstanceOf[Expression]
      }
      case _ => {
        val method = this.getClass().getMethods.find(_.getName == functionName)
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, specialField.level, specialField.index, specialField.arrayIndex).asInstanceOf[Expression]
      }
    }
  }

  def invokeEvalResolve(functionName : String, fieldAccess : FieldAccess) : Expression = {
    val method = this.getClass().getMethods.find(_.getName == functionName)
    if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
    method.get.invoke(this, fieldAccess).asInstanceOf[Expression]
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

  // evaluations and interpolations
  def evalAtLFace(fieldAccess : FieldAccess, dim : Int) = {
    val field = fieldAccess.fieldSelection.field
    val baseIndex = fieldAccess.index
    val level = field.level

    if ("cell" != field.discretization)
      Logger.warn(s"Attempting (${dimToString(dim)}-)face evaluation for non-cell based discretization (field ${field.identifier}, level ${field.level}, discretization ${field.discretization})")

    // prepare field accesses
    var centerFieldAccess = Duplicate(fieldAccess)
    var neighFieldAccess = Duplicate(fieldAccess)
    neighFieldAccess.index(dim) -= 1

    var centerIndex = Duplicate(baseIndex)
    var neighIndex = Duplicate(baseIndex)
    neighIndex(dim) -= 1

    // compile evaluation
    ((get_cell_center_to_face(level, neighIndex, None, dim) * centerFieldAccess + get_cell_center_to_face(level, centerIndex, None, dim) * neighFieldAccess)
      / get_stag_cv_width(level, Duplicate(centerIndex), None, dim))
  }

  def evalAtRFace(fieldAccess : FieldAccess, dim : Int) = {
    var modifiedAccess = Duplicate(fieldAccess)
    modifiedAccess.index(dim) += 1
    evalAtLFace(modifiedAccess, dim)
  }

  def evalAtEastFace(fieldAccess : FieldAccess) = evalAtRFace(fieldAccess, 0)
  def evalAtWestFace(fieldAccess : FieldAccess) = evalAtLFace(fieldAccess, 0)
  def evalAtNorthFace(fieldAccess : FieldAccess) = evalAtRFace(fieldAccess, 1)
  def evalAtSouthFace(fieldAccess : FieldAccess) = evalAtLFace(fieldAccess, 1)
  def evalAtTopFace(fieldAccess : FieldAccess) = evalAtRFace(fieldAccess, 2)
  def evalAtBottomFace(fieldAccess : FieldAccess) = evalAtLFace(fieldAccess, 2)
}

object ResolveSpecialFields extends DefaultStrategy("ResolveSpecialFields") {

  this += new Transformation("SearchAndReplace", {
    case specialField : SpecialFieldAccess => Grid.getGridObject.invokeAccessResolve(specialField)
  })
}

object ResolveGeometryFunctions extends DefaultStrategy("ResolveGeometryFunctions") {
  val specialFunctions = ListBuffer("evalAtEastFace", "evalAtWestFace", "evalAtNorthFace", "evalAtSouthFace", "evalAtTopFace", "evalAtBottomFace")

  this += new Transformation("SearchAndReplace", {
    case FunctionCallExpression(functionName, args) if specialFunctions.contains(functionName) =>
      Grid.getGridObject.invokeEvalResolve(functionName, args(0).asInstanceOf[FieldAccess])
  })
}

