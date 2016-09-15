package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.{ StatementList, _ }
import exastencils.logger._
import exastencils.prettyprinting._
import exastencils.strategies._
import exastencils.util._

case class IndexRange(var begin : IR_ExpressionIndex, var end : IR_ExpressionIndex) extends Node {
  def size = math.min(begin.size, end.size)

  def getTotalSize : IR_Expression = {
    var totalSize = (end - begin).reduce(_ * _)
    SimplifyStrategy.doUntilDoneStandalone(totalSize)
    totalSize
  }

  def print : String = {
    s"${ begin.prettyprint() } to ${ end.prettyprint() }"
  }
}

object Mapping {
  def resolveMultiIdx(layout : FieldLayout, index : IR_ExpressionIndex) : IR_Expression = {
    if (layout.numDimsData != index.length)
      Logger.warn(s"Index with dimensionality ${ index.length } does not match layout with dimensionality ${ layout.numDimsData }")

    val ret = (0 until math.min(layout.numDimsData, index.length)).map(dim => {
      val stride = ((0 until dim).map(d3 => layout.idxById("TOT", d3)).fold(1 : IR_Expression)(_ * _))
      index(dim) * stride
    }).fold(0 : IR_Expression)(_ + _)

    SimplifyExpression.simplifyIntegralExpr(ret)
  }

  def resolveMultiIdx(index : IR_ExpressionIndex, aabb : IndexRange) : IR_Expression = resolveMultiIdx(index, IR_ExpressionIndex(aabb.end, aabb.begin, _ - _))
  def resolveMultiIdx(index : IR_ExpressionIndex, strides : IR_ExpressionIndex) : IR_Expression = {
    if (strides.length != index.length) Logger.warn(s"Index with dimensionality ${ index.length } does not match strides with dimensionality ${ strides.length }")

    val ret = (0 until math.min(strides.length, index.length)).map(dim => {
      val stride = ((0 until dim).map(d3 => strides(d3)).fold(1 : IR_Expression)(_ * _))
      index(dim) * stride
    }).fold(0 : IR_Expression)(_ + _)

    SimplifyExpression.simplifyIntegralExpr(ret)
  }
}

object dimToString extends (Int => String) {
  // FIXME: this is named inappropriately; move this to a global variable manager as it becomes available; rename to i_x after checking where x, etc are used explicitly
  override def apply(dim : Int) : String = {
    return dim match {
      case 0 => "x"
      case 1 => "y"
      case 2 => "z"
      case 3 => "w"
      case 4 => "v"
      case 5 => "u"
      case _ => "UNKNOWN"
    }
  }
}

case class InitGeomCoords(var field : Field, var directCoords : Boolean, var offset : IR_ExpressionIndex = IR_ExpressionIndex(0, 0, 0) /* was float index before */) extends IR_Statement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = InitGeomCoords\n"

  override def expand : Output[StatementList] = {
    if (Knowledge.domain_fragmentTransformation) {
      // TODO: integrate into the new grid class family
      ListBuffer[IR_Statement](
        IR_VariableDeclaration(IR_RealDatatype, "xPosTMP", field.fieldLayout.discretization match {
          case "node" | "face_x"            =>
            Some(((if (directCoords) ("x" - field.referenceOffset(0)) else ("x" : IR_Expression)) + offset(0))
              / CastExpression(IR_RealDatatype, field.fieldLayout.idxById("DRE", 0) - field.fieldLayout.idxById("DLB", 0) - 1)
              * (IR_ArrayAccess(iv.PrimitivePositionEnd(), 0) - IR_ArrayAccess(iv.PrimitivePositionBegin(), 0)) + IR_ArrayAccess(iv.PrimitivePositionBegin(), 0))
          case "cell" | "face_y" | "face_z" =>
            Some(((if (directCoords) ("x" - field.referenceOffset(0)) else ("x" : IR_Expression)) + 0.5 + offset(0))
              / CastExpression(IR_RealDatatype, field.fieldLayout.idxById("DRE", 0) - field.fieldLayout.idxById("DLB", 0) - 0)
              * (IR_ArrayAccess(iv.PrimitivePositionEnd(), 0) - IR_ArrayAccess(iv.PrimitivePositionBegin(), 0)) + IR_ArrayAccess(iv.PrimitivePositionBegin(), 0))
        }),
        IR_VariableDeclaration(IR_RealDatatype, "yPosTMP",
          if (Knowledge.dimensionality > 1) {
            field.fieldLayout.discretization match {
              case "node" | "face_y"            =>
                (((if (directCoords) ("y" - field.referenceOffset(1)) else ("y" : IR_Expression)) + offset(1))
                  / CastExpression(IR_RealDatatype, field.fieldLayout.idxById("DRE", 1) - field.fieldLayout.idxById("DLB", 1) - 1)
                  * (IR_ArrayAccess(iv.PrimitivePositionEnd(), 1) - IR_ArrayAccess(iv.PrimitivePositionBegin(), 1)) + IR_ArrayAccess(iv.PrimitivePositionBegin(), 1))
              case "cell" | "face_x" | "face_z" =>
                (((if (directCoords) ("y" - field.referenceOffset(1)) else ("y" : IR_Expression)) + 0.5 + offset(1))
                  / CastExpression(IR_RealDatatype, field.fieldLayout.idxById("DRE", 1) - field.fieldLayout.idxById("DLB", 1) - 0)
                  * (IR_ArrayAccess(iv.PrimitivePositionEnd(), 1) - IR_ArrayAccess(iv.PrimitivePositionBegin(), 1)) + IR_ArrayAccess(iv.PrimitivePositionBegin(), 1))
            }
          } else IR_IntegerConstant(1)),
        IR_VariableDeclaration(IR_RealDatatype, "zPosTMP",
          if (Knowledge.dimensionality > 2) {
            field.fieldLayout.discretization match {
              case "node" | "face_z"            =>
                (((if (directCoords) ("z" - field.referenceOffset(2)) else ("z" : IR_Expression)) + offset(2))
                  / CastExpression(IR_RealDatatype, field.fieldLayout.idxById("DRE", 2) - field.fieldLayout.idxById("DLB", 2) - 1)
                  * (IR_ArrayAccess(iv.PrimitivePositionEnd(), 2) - IR_ArrayAccess(iv.PrimitivePositionBegin(), 2)) + IR_ArrayAccess(iv.PrimitivePositionBegin(), 2))
              case "cell" | "face_x" | "face_y" =>
                (((if (directCoords) ("z" - field.referenceOffset(2)) else ("z" : IR_Expression)) + 0.5 + offset(2))
                  / CastExpression(IR_RealDatatype, field.fieldLayout.idxById("DRE", 2) - field.fieldLayout.idxById("DLB", 2) - 0)
                  * (IR_ArrayAccess(iv.PrimitivePositionEnd(), 2) - IR_ArrayAccess(iv.PrimitivePositionBegin(), 2)) + IR_ArrayAccess(iv.PrimitivePositionBegin(), 2))
            }
          } else IR_IntegerConstant(1)),
        IR_VariableDeclaration(IR_RealDatatype, "xPos", Some(
          ("xPosTMP" : IR_Expression) * IR_ArrayAccess(iv.PrimitiveTransformation(), 0)
            + ("yPosTMP" : IR_Expression) * IR_ArrayAccess(iv.PrimitiveTransformation(), 1)
            + ("zPosTMP" : IR_Expression) * IR_ArrayAccess(iv.PrimitiveTransformation(), 2)
            + IR_ArrayAccess(iv.PrimitiveTransformation(), 3))),
        if (Knowledge.dimensionality > 1)
          IR_VariableDeclaration(IR_RealDatatype, "yPos", Some(
            ("xPosTMP" : IR_Expression) * IR_ArrayAccess(iv.PrimitiveTransformation(), 4)
              + ("yPosTMP" : IR_Expression) * IR_ArrayAccess(iv.PrimitiveTransformation(), 5)
              + ("zPosTMP" : IR_Expression) * IR_ArrayAccess(iv.PrimitiveTransformation(), 6)
              + IR_ArrayAccess(iv.PrimitiveTransformation(), 7)))
        else IR_NullStatement,
        if (Knowledge.dimensionality > 2)
          IR_VariableDeclaration(IR_RealDatatype, "zPos", Some(
            ("xPosTMP" : IR_Expression) * IR_ArrayAccess(iv.PrimitiveTransformation(), 8)
              + ("yPosTMP" : IR_Expression) * IR_ArrayAccess(iv.PrimitiveTransformation(), 9)
              + ("zPosTMP" : IR_Expression) * IR_ArrayAccess(iv.PrimitiveTransformation(), 10)
              + IR_ArrayAccess(iv.PrimitiveTransformation(), 11)))
        else IR_NullStatement)
    } else {
      Logger.error("deprecated")
    }
  }
}

object ResolveCoordinates extends DefaultStrategy("ResolveCoordinates") {
  var replacement : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // to be overwritten

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

  Knowledge.dimensionality match { // TODO: update and extend -> arbitrary dimensionality, VariableAccesses and name of indices
    case 1 => this += new Transformation("SearchAndReplace", {
      case IR_StringLiteral("x") => replacement(0)
    })
    case 2 => this += new Transformation("SearchAndReplace", {
      case IR_StringLiteral("x") => replacement(0)
      case IR_StringLiteral("y") => replacement(1)
    })
    case 3 => this += new Transformation("SearchAndReplace", {
      case IR_StringLiteral("x") => replacement(0)
      case IR_StringLiteral("y") => replacement(1)
      case IR_StringLiteral("z") => replacement(2)
    })
  }
}

object CreateGeomCoordinates extends DefaultStrategy("Add geometric coordinate calculations") {
  this += new Transformation("Search and extend", {
    case loop : IR_LoopOverPointsInOneFragment =>
      if (StateManager.findFirst[AnyRef]((node : Any) => node match {
        case IR_StringLiteral("xPos") | IR_StringLiteral("yPos") | IR_StringLiteral("zPos")             => true
        case IR_VariableAccess("xPos", _) | IR_VariableAccess("yPos", _) | IR_VariableAccess("zPos", _) => true
        case _                                                                                          => false
      }, loop).isDefined) {
        loop.body.prepend(new InitGeomCoords(loop.field, false))
      }

      loop
  })
}
