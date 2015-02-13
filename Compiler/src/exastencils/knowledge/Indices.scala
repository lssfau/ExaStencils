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
    if (field.fieldLayout.nodeBased) {
      ListBuffer[Statement](
        VariableDeclarationStatement(new RealDatatype, "xPos", Some(
          ((if (directCoords) ("x" - field.referenceOffset.index_0) else ("x" : Expression)) + offset.index_0)
            / FloatConstant(field.fieldLayout(0).idxDupRightEnd - field.fieldLayout(0).idxDupLeftBegin - 1)
            * (ArrayAccess(iv.PrimitivePositionEnd(), 0) - ArrayAccess(iv.PrimitivePositionBegin(), 0)) + ArrayAccess(iv.PrimitivePositionBegin(), 0))),
        if (Knowledge.dimensionality > 1)
          VariableDeclarationStatement(new RealDatatype, "yPos", Some(
          ((if (directCoords) ("y" - field.referenceOffset.index_1) else ("y" : Expression)) + offset.index_1)
            / FloatConstant(field.fieldLayout(1).idxDupRightEnd - field.fieldLayout(1).idxDupLeftBegin - 1)
            * (ArrayAccess(iv.PrimitivePositionEnd(), 1) - ArrayAccess(iv.PrimitivePositionBegin(), 1)) + ArrayAccess(iv.PrimitivePositionBegin(), 1)))
        else NullStatement,
        if (Knowledge.dimensionality > 2)
          VariableDeclarationStatement(new RealDatatype, "zPos", Some(
          ((if (directCoords) ("z" - field.referenceOffset.index_2) else ("z" : Expression)) + offset.index_2)
            / FloatConstant(field.fieldLayout(2).idxDupRightEnd - field.fieldLayout(2).idxDupLeftBegin - 1)
            * (ArrayAccess(iv.PrimitivePositionEnd(), 2) - ArrayAccess(iv.PrimitivePositionBegin(), 2)) + ArrayAccess(iv.PrimitivePositionBegin(), 2)))
        else NullStatement)
    } else {
      ListBuffer[Statement](
        VariableDeclarationStatement(new RealDatatype, "xPos", Some(
          ((if (directCoords) ("x" - field.referenceOffset.index_0) else ("x" : Expression)) + 0.5 + offset.index_0)
            / FloatConstant(field.fieldLayout(0).idxDupRightEnd - field.fieldLayout(0).idxDupLeftBegin - 0)
            * (ArrayAccess(iv.PrimitivePositionEnd(), 0) - ArrayAccess(iv.PrimitivePositionBegin(), 0)) + ArrayAccess(iv.PrimitivePositionBegin(), 0))),
        if (Knowledge.dimensionality > 1)
          VariableDeclarationStatement(new RealDatatype, "yPos", Some(
          ((if (directCoords) ("y" - field.referenceOffset.index_1) else ("y" : Expression)) + 0.5 + offset.index_1)
            / FloatConstant(field.fieldLayout(1).idxDupRightEnd - field.fieldLayout(1).idxDupLeftBegin - 0)
            * (ArrayAccess(iv.PrimitivePositionEnd(), 1) - ArrayAccess(iv.PrimitivePositionBegin(), 1)) + ArrayAccess(iv.PrimitivePositionBegin(), 1)))
        else NullStatement,
        if (Knowledge.dimensionality > 2)
          VariableDeclarationStatement(new RealDatatype, "zPos", Some(
          ((if (directCoords) ("z" - field.referenceOffset.index_2) else ("z" : Expression)) + 0.5 + offset.index_2)
            / FloatConstant(field.fieldLayout(2).idxDupRightEnd - field.fieldLayout(2).idxDupLeftBegin - 0)
            * (ArrayAccess(iv.PrimitivePositionEnd(), 2) - ArrayAccess(iv.PrimitivePositionBegin(), 2)) + ArrayAccess(iv.PrimitivePositionBegin(), 2)))
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

object CreateGeomCoordinates extends DefaultStrategy("Add geometric coodrinate calculations") {
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
