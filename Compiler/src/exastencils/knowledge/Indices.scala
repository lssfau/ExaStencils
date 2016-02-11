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
    var size = (end - begin).reduce(_ * _)
    SimplifyStrategy.doUntilDoneStandalone(size)
    size
  }
}

object Mapping {
  def resolveMultiIdx(layout : FieldLayout, index : MultiIndex) : Expression = {
    if (layout.numDimsData != index.length) Logger.warn(s"Index with dimensionality ${index.length} does not match layout with dimensionality ${layout.numDimsData}")

    var ret = (0 until math.min(layout.numDimsData, index.length)).map(dim => {
      val stride = ((0 until dim).map(d3 => layout(d3).total).fold(1 : Expression)(_ * _))
      index(dim) * stride
    }).fold(0 : Expression)(_ + _)

    if (Knowledge.data_genVariableFieldSizes) {
      SimplifyStrategy.doUntilDoneStandalone(ret)
      ret
    } else {
      SimplifyExpression.simplifyIntegralExpr(ret)
    }
  }

  def resolveMultiIdx(index : MultiIndex, aabb : IndexRange) : Expression = resolveMultiIdx(index, new MultiIndex(aabb.end, aabb.begin, _ - _))
  def resolveMultiIdx(index : MultiIndex, strides : MultiIndex) : Expression = {
    if (strides.length != index.length) Logger.warn(s"Index with dimensionality ${index.length} does not match strides with dimensionality ${strides.length}")

    var ret = (0 until math.min(strides.length, index.length)).map(dim => {
      val stride = ((0 until dim).map(d3 => strides(d3)).fold(1 : Expression)(_ * _))
      index(dim) * stride
    }).fold(0 : Expression)(_ + _)

    if (Knowledge.data_genVariableFieldSizes) {
      SimplifyStrategy.doUntilDoneStandalone(ret)
      ret
    } else {
      SimplifyExpression.simplifyIntegralExpr(ret)
    }
  }
}

object dimToString extends (Int => String) {
  // FIXME: this is named inappropriately; move this to a global variable manager as it becomes available
  override def apply(dim : Int) : String = {
    return dim match {
      case 0 => "x"
      case 1 => "y"
      case 2 => "z"
      case 3 => "w"
      case _ => "UNKNOWN"
    }
  }
}

case class InitGeomCoords(var field : Field, var directCoords : Boolean, var offset : MultiIndex = new MultiIndex(0, 0, 0) /* was float index before */ ) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = InitGeomCoords\n"

  override def expand : Output[StatementList] = {
    if (Knowledge.domain_fragmentTransformation) {
      // TODO: integrate into the new grid class family
      ListBuffer[Statement](
        VariableDeclarationStatement(RealDatatype, "xPosTMP", field.fieldLayout.discretization match {
          case "node" | "face_x" =>
            Some(((if (directCoords) ("x" - field.referenceOffset(0)) else ("x" : Expression)) + offset(0))
              / CastExpression(RealDatatype, field.fieldLayout.idxById("DRE", 0) - field.fieldLayout.idxById("DLB", 0) - 1)
              * (ArrayAccess(iv.PrimitivePositionEnd(), 0) - ArrayAccess(iv.PrimitivePositionBegin(), 0)) + ArrayAccess(iv.PrimitivePositionBegin(), 0))
          case "cell" | "face_y" | "face_z" =>
            Some(((if (directCoords) ("x" - field.referenceOffset(0)) else ("x" : Expression)) + 0.5 + offset(0))
              / CastExpression(RealDatatype, field.fieldLayout.idxById("DRE", 0) - field.fieldLayout.idxById("DLB", 0) - 0)
              * (ArrayAccess(iv.PrimitivePositionEnd(), 0) - ArrayAccess(iv.PrimitivePositionBegin(), 0)) + ArrayAccess(iv.PrimitivePositionBegin(), 0))
        }),
        VariableDeclarationStatement(RealDatatype, "yPosTMP",
          if (Knowledge.dimensionality > 1) {
            field.fieldLayout.discretization match {
              case "node" | "face_y" =>
                Some(((if (directCoords) ("y" - field.referenceOffset(1)) else ("y" : Expression)) + offset(1))
                  / CastExpression(RealDatatype, field.fieldLayout.idxById("DRE", 1) - field.fieldLayout.idxById("DLB", 1) - 1)
                  * (ArrayAccess(iv.PrimitivePositionEnd(), 1) - ArrayAccess(iv.PrimitivePositionBegin(), 1)) + ArrayAccess(iv.PrimitivePositionBegin(), 1))
              case "cell" | "face_x" | "face_z" =>
                Some(((if (directCoords) ("y" - field.referenceOffset(1)) else ("y" : Expression)) + 0.5 + offset(1))
                  / CastExpression(RealDatatype, field.fieldLayout.idxById("DRE", 1) - field.fieldLayout.idxById("DLB", 1) - 0)
                  * (ArrayAccess(iv.PrimitivePositionEnd(), 1) - ArrayAccess(iv.PrimitivePositionBegin(), 1)) + ArrayAccess(iv.PrimitivePositionBegin(), 1))
            }
          } else Some(1)),
        VariableDeclarationStatement(RealDatatype, "zPosTMP",
          if (Knowledge.dimensionality > 2) {
            field.fieldLayout.discretization match {
              case "node" | "face_z" =>
                Some(((if (directCoords) ("z" - field.referenceOffset(2)) else ("z" : Expression)) + offset(2))
                  / CastExpression(RealDatatype, field.fieldLayout.idxById("DRE", 2) - field.fieldLayout.idxById("DLB", 2) - 1)
                  * (ArrayAccess(iv.PrimitivePositionEnd(), 2) - ArrayAccess(iv.PrimitivePositionBegin(), 2)) + ArrayAccess(iv.PrimitivePositionBegin(), 2))
              case "cell" | "face_x" | "face_y" =>
                Some(((if (directCoords) ("z" - field.referenceOffset(2)) else ("z" : Expression)) + 0.5 + offset(2))
                  / CastExpression(RealDatatype, field.fieldLayout.idxById("DRE", 2) - field.fieldLayout.idxById("DLB", 2) - 0)
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
      Logger.error("deprecated")
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
      case StringLiteral("x") => replacement(0)
    })
    case 2 => this += new Transformation("SearchAndReplace", {
      case StringLiteral("x") => replacement(0)
      case StringLiteral("y") => replacement(1)
    })
    case 3 => this += new Transformation("SearchAndReplace", {
      case StringLiteral("x") => replacement(0)
      case StringLiteral("y") => replacement(1)
      case StringLiteral("z") => replacement(2)
    })
  }
}

object CreateGeomCoordinates extends DefaultStrategy("Add geometric coordinate calculations") {
  this += new Transformation("Search and extend", {
    case loop : LoopOverPointsInOneFragment =>
      if (StateManager.findFirst[AnyRef]((node : Any) => node match {
        case StringLiteral("xPos") | StringLiteral("yPos") | StringLiteral("zPos") => true
        case VariableAccess("xPos", _) | VariableAccess("yPos", _) | VariableAccess("zPos", _) => true
        case _ => false
      }, loop).isDefined) {
        loop.body.prepend(new InitGeomCoords(loop.field, false))
      }

      loop
  })
}
