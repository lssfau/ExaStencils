package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.StatementList
import exastencils.logger._
import exastencils.prettyprinting._
import exastencils.strategies._
import exastencils.util._

case class IndexRange(var begin : MultiIndex = new MultiIndex, var end : MultiIndex = new MultiIndex) extends Node {
  def getSize : Expression = {
    var size = DimArray().map(i => (end(i) - begin(i)).asInstanceOf[Expression]).reduceLeft(_ * _)
    SimplifyStrategy.doUntilDoneStandalone(size)
    size
  }
  def getSizeHigher : Expression = {
    var size = DimArrayHigher().map(i => (end(i) - begin(i)).asInstanceOf[Expression]).reduceLeft(_ * _)
    SimplifyStrategy.doUntilDoneStandalone(size)
    size
  }
}

object Mapping {
  def resolveMultiIdx(layout : FieldLayout, index : MultiIndex) : Expression = {
    val ret = Knowledge.dimensionality match {
      case 0 => (index(0))
      case 1 => (index(1) * layout(0).total + index(0))
      case 2 => (index(2) * (layout(1).total * layout(0).total) + index(1) * layout(0).total + index(0))
      case 3 => (index(3) * (layout(2).total * layout(1).total * layout(0).total) + index(2) * (layout(1).total * layout(0).total) + index(1) * layout(0).total + index(0))
    }
    if (Knowledge.experimental_useLevelIndepFcts) {
      SimplifyStrategy.doUntilDoneStandalone(ret)
      ret
    } else {
      SimplifyExpression.simplifyIntegralExpr(ret)
    }
  }

  def resolveMultiIdx(index : MultiIndex, aabb : IndexRange) : Expression = {
    val ret = Knowledge.dimensionality match {
      case 0 => (index(0))
      case 1 => (index(1) * (aabb.end(0) - aabb.begin(0)) + index(0))
      case 2 => (index(2) * ((aabb.end(1) - aabb.begin(1)) * (aabb.end(0) - aabb.begin(0))) + index(1) * (aabb.end(0) - aabb.begin(0)) + index(0))
      case 3 => (index(3) * ((aabb.end(2) - aabb.begin(2)) * (aabb.end(1) - aabb.begin(1)) * (aabb.end(0) - aabb.begin(0))) + index(2) * ((aabb.end(1) - aabb.begin(1)) * (aabb.end(0) - aabb.begin(0))) + index(1) * (aabb.end(0) - aabb.begin(0)) + index(0))
    }
    if (Knowledge.experimental_useLevelIndepFcts) {
      SimplifyStrategy.doUntilDoneStandalone(ret)
      ret
    } else {
      SimplifyExpression.simplifyIntegralExpr(ret)
    }
  }
}

object DimArray {
  def apply() : Array[Int] = { (0 until Knowledge.dimensionality).toArray }
}

object DimArrayHigher {
  def apply() : Array[Int] = { (0 until Knowledge.dimensionality + 1).toArray }
}

object dimToString extends (Int => String) {
  // FIXME: this is named inappropriately; move this to a global variable manager as it becomes available
  def apply(dim : Int) : String = {
    return dim match {
      case 0 => "x"
      case 1 => "y"
      case 2 => "z"
      case 3 => "w"
      case _ => "UNKNOWN"
    }
  }
}

case class InitGeomCoords(var field : Field, var directCoords : Boolean, var offset : MultiIndex = MultiIndex(0.0, 0.0, 0.0)) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = InitGeomCoords\n"

  override def expand : Output[StatementList] = {
    if (Knowledge.domain_fragmentTransformation) {
      ListBuffer[Statement](
        VariableDeclarationStatement(RealDatatype, "xPosTMP", field.fieldLayout.discretization match {
          case "node" | "face_x" =>
            Some(((if (directCoords) ("x" - field.referenceOffset.index_0) else ("x" : Expression)) + offset.index_0)
              / FloatConstant(field.fieldLayout(0).idxDupRightEnd - field.fieldLayout(0).idxDupLeftBegin - 1)
              * (ArrayAccess(iv.PrimitivePositionEnd(), 0) - ArrayAccess(iv.PrimitivePositionBegin(), 0)) + ArrayAccess(iv.PrimitivePositionBegin(), 0))
          case "cell" | "face_y" | "face_z" =>
            Some(((if (directCoords) ("x" - field.referenceOffset.index_0) else ("x" : Expression)) + 0.5 + offset.index_0)
              / FloatConstant(field.fieldLayout(0).idxDupRightEnd - field.fieldLayout(0).idxDupLeftBegin - 0)
              * (ArrayAccess(iv.PrimitivePositionEnd(), 0) - ArrayAccess(iv.PrimitivePositionBegin(), 0)) + ArrayAccess(iv.PrimitivePositionBegin(), 0))
        }),
        VariableDeclarationStatement(RealDatatype, "yPosTMP",
          if (Knowledge.dimensionality > 1) {
            field.fieldLayout.discretization match {
              case "node" | "face_y" =>
                Some(((if (directCoords) ("y" - field.referenceOffset.index_1) else ("y" : Expression)) + offset.index_1)
                  / FloatConstant(field.fieldLayout(1).idxDupRightEnd - field.fieldLayout(1).idxDupLeftBegin - 1)
                  * (ArrayAccess(iv.PrimitivePositionEnd(), 1) - ArrayAccess(iv.PrimitivePositionBegin(), 1)) + ArrayAccess(iv.PrimitivePositionBegin(), 1))
              case "cell" | "face_x" | "face_z" =>
                Some(((if (directCoords) ("y" - field.referenceOffset.index_1) else ("y" : Expression)) + 0.5 + offset.index_1)
                  / FloatConstant(field.fieldLayout(1).idxDupRightEnd - field.fieldLayout(1).idxDupLeftBegin - 0)
                  * (ArrayAccess(iv.PrimitivePositionEnd(), 1) - ArrayAccess(iv.PrimitivePositionBegin(), 1)) + ArrayAccess(iv.PrimitivePositionBegin(), 1))
            }
          } else Some(1)),
        VariableDeclarationStatement(RealDatatype, "zPosTMP",
          if (Knowledge.dimensionality > 2) {
            field.fieldLayout.discretization match {
              case "node" | "face_z" =>
                Some(((if (directCoords) ("z" - field.referenceOffset.index_2) else ("z" : Expression)) + offset.index_2)
                  / FloatConstant(field.fieldLayout(2).idxDupRightEnd - field.fieldLayout(2).idxDupLeftBegin - 1)
                  * (ArrayAccess(iv.PrimitivePositionEnd(), 2) - ArrayAccess(iv.PrimitivePositionBegin(), 2)) + ArrayAccess(iv.PrimitivePositionBegin(), 2))
              case "cell" | "face_x" | "face_y" =>
                Some(((if (directCoords) ("z" - field.referenceOffset.index_2) else ("z" : Expression)) + 0.5 + offset.index_2)
                  / FloatConstant(field.fieldLayout(2).idxDupRightEnd - field.fieldLayout(2).idxDupLeftBegin - 0)
                  * (ArrayAccess(iv.PrimitivePositionEnd(), 2) - ArrayAccess(iv.PrimitivePositionBegin(), 2)) + ArrayAccess(iv.PrimitivePositionBegin(), 2))
            }
          } else Some(1)),
        VariableDeclarationStatement(RealDatatype, "xPos", Some(
          ("xPosTMP" : Expression) * ArrayAccess(iv.PrimitiveTransformation(), 0)
            + ("yPosTMP" : Expression) * ArrayAccess(iv.PrimitiveTransformation(), 1)
            + ("zPosTMP" : Expression) * ArrayAccess(iv.PrimitiveTransformation(), 2)
            + ArrayAccess(iv.PrimitiveTransformation(), 3))),
        if (Knowledge.dimensionality > 1)
          VariableDeclarationStatement(RealDatatype, "yPos", Some(
          ("xPosTMP" : Expression) * ArrayAccess(iv.PrimitiveTransformation(), 4)
            + ("yPosTMP" : Expression) * ArrayAccess(iv.PrimitiveTransformation(), 5)
            + ("zPosTMP" : Expression) * ArrayAccess(iv.PrimitiveTransformation(), 6)
            + ArrayAccess(iv.PrimitiveTransformation(), 7)))
        else NullStatement,
        if (Knowledge.dimensionality > 2)
          VariableDeclarationStatement(RealDatatype, "zPos", Some(
          ("xPosTMP" : Expression) * ArrayAccess(iv.PrimitiveTransformation(), 8)
            + ("yPosTMP" : Expression) * ArrayAccess(iv.PrimitiveTransformation(), 9)
            + ("zPosTMP" : Expression) * ArrayAccess(iv.PrimitiveTransformation(), 10)
            + ArrayAccess(iv.PrimitiveTransformation(), 11)))
        else NullStatement)
    } else {
      ListBuffer[Statement](
        VariableDeclarationStatement(RealDatatype, "xPos", field.fieldLayout.discretization match {
          case "node" | "face_x" =>
            Some(((if (directCoords) ("x" - field.referenceOffset.index_0) else ("x" : Expression)) + offset.index_0)
              / FloatConstant(field.fieldLayout(0).idxDupRightEnd - field.fieldLayout(0).idxDupLeftBegin - 1)
              * (ArrayAccess(iv.PrimitivePositionEnd(), 0) - ArrayAccess(iv.PrimitivePositionBegin(), 0)) + ArrayAccess(iv.PrimitivePositionBegin(), 0))
          case "cell" | "face_y" | "face_z" =>
            Some(((if (directCoords) ("x" - field.referenceOffset.index_0) else ("x" : Expression)) + 0.5 + offset.index_0)
              / FloatConstant(field.fieldLayout(0).idxDupRightEnd - field.fieldLayout(0).idxDupLeftBegin - 0)
              * (ArrayAccess(iv.PrimitivePositionEnd(), 0) - ArrayAccess(iv.PrimitivePositionBegin(), 0)) + ArrayAccess(iv.PrimitivePositionBegin(), 0))
        }),
        if (Knowledge.dimensionality > 1)
          VariableDeclarationStatement(RealDatatype, "yPos", field.fieldLayout.discretization match {
          case "node" | "face_y" =>
            Some(((if (directCoords) ("y" - field.referenceOffset.index_1) else ("y" : Expression)) + offset.index_1)
              / FloatConstant(field.fieldLayout(1).idxDupRightEnd - field.fieldLayout(1).idxDupLeftBegin - 1)
              * (ArrayAccess(iv.PrimitivePositionEnd(), 1) - ArrayAccess(iv.PrimitivePositionBegin(), 1)) + ArrayAccess(iv.PrimitivePositionBegin(), 1))
          case "cell" | "face_x" | "face_z" =>
            Some(((if (directCoords) ("y" - field.referenceOffset.index_1) else ("y" : Expression)) + 0.5 + offset.index_1)
              / FloatConstant(field.fieldLayout(1).idxDupRightEnd - field.fieldLayout(1).idxDupLeftBegin - 0)
              * (ArrayAccess(iv.PrimitivePositionEnd(), 1) - ArrayAccess(iv.PrimitivePositionBegin(), 1)) + ArrayAccess(iv.PrimitivePositionBegin(), 1))
        })
        else NullStatement,
        if (Knowledge.dimensionality > 2)
          VariableDeclarationStatement(RealDatatype, "zPos", field.fieldLayout.discretization match {
          case "node" | "face_z" =>
            Some(((if (directCoords) ("z" - field.referenceOffset.index_2) else ("z" : Expression)) + offset.index_2)
              / FloatConstant(field.fieldLayout(2).idxDupRightEnd - field.fieldLayout(2).idxDupLeftBegin - 1)
              * (ArrayAccess(iv.PrimitivePositionEnd(), 2) - ArrayAccess(iv.PrimitivePositionBegin(), 2)) + ArrayAccess(iv.PrimitivePositionBegin(), 2))
          case "cell" | "face_x" | "face_y" =>
            Some(((if (directCoords) ("z" - field.referenceOffset.index_2) else ("z" : Expression)) + 0.5 + offset.index_2)
              / FloatConstant(field.fieldLayout(2).idxDupRightEnd - field.fieldLayout(2).idxDupLeftBegin - 0)
              * (ArrayAccess(iv.PrimitivePositionEnd(), 2) - ArrayAccess(iv.PrimitivePositionBegin(), 2)) + ArrayAccess(iv.PrimitivePositionBegin(), 2))
        })
        else NullStatement)
    }
  }
}

object ResolveCoordinates extends DefaultStrategy("ResolveCoordinates") {
  var replacement : MultiIndex = LoopOverDimensions.defIt

  def doUntilDone(node : Option[Node] = None) = {
    do { apply(node) }
    while (results.last._2.matches > 0) // FIXME: cleaner code
  }

  def doUntilDoneStandalone(node : Node) = {
    val oldLvl = Logger.getLevel
    Logger.setLevel(Logger.WARNING)
    do { applyStandalone(node) }
    while (results.last._2.matches > 0) // FIXME: cleaner code
    Logger.setLevel(oldLvl)
  }

  Knowledge.dimensionality match {
    case 1 => this += new Transformation("SearchAndReplace", {
      case StringConstant("x") => replacement(0)
    })
    case 2 => this += new Transformation("SearchAndReplace", {
      case StringConstant("x") => replacement(0)
      case StringConstant("y") => replacement(1)
    })
    case 3 => this += new Transformation("SearchAndReplace", {
      case StringConstant("x") => replacement(0)
      case StringConstant("y") => replacement(1)
      case StringConstant("z") => replacement(2)
    })
  }
}

object CreateGeomCoordinates extends DefaultStrategy("Add geometric coordinate calculations") {
  this += new Transformation("Search and extend", {
    case loop : LoopOverPointsInOneFragment =>
      if (StateManager.findFirst[AnyRef]((node : Any) => node match {
        case StringConstant("xPos") | StringConstant("yPos") | StringConstant("zPos") => true
        case VariableAccess("xPos", _) | VariableAccess("yPos", _) | VariableAccess("zPos", _) => true
        case _ => false
      }, loop).isDefined) {
        loop.body.prepend(new InitGeomCoords(loop.field, false))
      }

      loop
  })
}

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