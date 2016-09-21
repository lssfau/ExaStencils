package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.logger._
import exastencils.prettyprinting._
import exastencils.stencil.ir._
import exastencils.stencil.l4._

trait Number extends L4_Expression {
  def value : AnyVal
}

case class VectorExpression(var datatype : Option[L4_Datatype], var expressions : List[L4_Expression], var rowVector : Option[Boolean]) extends L4_Expression {
  // rowVector == true: Row; false: Column; None: unspecified
  def length = expressions.length

  def apply(i : Integer) = expressions(i)
  def isConstant = expressions.filter(e => e.isInstanceOf[Number]).length == expressions.length

  def prettyprint(out : PpStream) = {
    out << '{' <<< (expressions, ", ") << '}'
    if (rowVector.getOrElse(true) == false) {
      out << 'T';
    }
  }
  def progress = IR_VectorExpression(if (datatype.isDefined) Some(datatype.get.progress); else None, expressions.map(_.progress).to[ListBuffer], rowVector)
}

object VectorExpression {
  // helper function
  def isRowVector(n : Node) = {
    if (n.isInstanceOf[VectorExpression]) {
      var v = n.asInstanceOf[VectorExpression]
      if (v.rowVector.getOrElse(true)) true; else false
    } else {
      false
    }
  }
  def isColumnVector(n : Node) = {
    if (n.isInstanceOf[VectorExpression]) {
      var v = n.asInstanceOf[VectorExpression]
      if (v.rowVector.getOrElse(true)) true; else false
    } else {
      false
    }
  }
}

case class MatrixExpression(var datatype : Option[L4_Datatype], var expressions : List[VectorExpression]) extends L4_Expression {
  if (expressions.filter(x => x.length != expressions(0).length).length > 0) {
    Logger.error("Rows of matrix must be of equal length")
  }

  def prettyprint(out : PpStream) = { out << '{'; expressions.foreach(e => { e.prettyprint(out); out << ",\n" }); out << "} '" }

  def progress = IR_MatrixExpression(if (datatype.isDefined) Some(datatype.get.progress); else None, expressions.map(_.expressions.map(_.progress).to[ListBuffer]).to[ListBuffer])

  def rows = expressions.length
  def columns = expressions(0).length
  def isConstant = expressions.filter(_.isConstant).length == expressions.length
}

abstract class Access() extends L4_Expression {
  def name : String
}

case class UnresolvedAccess(var name : String,
    var slot : Option[L4_SlotSpecification],
    var level : Option[L4_AccessLevelSpecification],
    var offset : Option[L4_ExpressionIndex],
    var arrayIndex : Option[Int],
    var dirAccess : Option[L4_ExpressionIndex]) extends Access {
  def prettyprint(out : PpStream) = {
    out << name
    if (slot.isDefined) out << '[' << slot.get << ']'
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (dirAccess.isDefined) out << ':' << dirAccess
  }

  def progress : IR_Expression = {
    // IR_StringLiteral("ERROR - Unresolved Access")
    Logger.warn(s"Progressing UnresolvedAccess $name")
    resolveToBasicOrLeveledAccess.progress
  }

  def resolveToBasicOrLeveledAccess = {
    if (slot.isDefined) Logger.warn("Discarding meaningless slot access on basic or leveled access")
    if (offset.isDefined) Logger.warn("Discarding meaningless offset access on basic or leveled access")
    if (arrayIndex.isDefined) Logger.warn("Discarding meaningless array index access on basic or leveled access")
    if (dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on basic or leveled access " + name)
    if (level.isDefined) LeveledAccess(name, level.get) else BasicAccess(name)
  }
}

case class BasicAccess(var name : String) extends Access {
  def prettyprint(out : PpStream) = { out << name }
  def progress : IR_StringLiteral = { IR_StringLiteral(name) }
}

case class LeveledAccess(var name : String, var level : L4_AccessLevelSpecification) extends Access {
  def prettyprint(out : PpStream) = { out << name << '[' << level << ']' }

  def progress : IR_Expression = {
    IR_StringLiteral(name + "_" + level.resolveLevel)
  }
}

case class StencilConvolution(var stencilAccess : L4_StencilAccess, var fieldAccess : L4_FieldAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = { out << stencilAccess << " * " << fieldAccess }

  def progress : IR_StencilConvolution = {
    IR_StencilConvolution(stencilAccess.getBasicStencilAccess.stencil, fieldAccess.progress)
  }
}

case class StencilFieldConvolution(var stencilFieldAccess : L4_StencilFieldAccess, var fieldAccess : L4_FieldAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = { out << stencilFieldAccess << " * " << fieldAccess }

  def progress : IR_StencilFieldConvolution = {
    IR_StencilFieldConvolution(stencilFieldAccess.getBasicStencilFieldAccess, fieldAccess.progress)
  }
}

case class StencilStencilConvolution(var stencilLeft : L4_StencilAccess, var stencilRight : L4_StencilAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = { out << stencilLeft << " * " << stencilRight }

  def progress : IR_StencilStencilConvolution = {
    IR_StencilStencilConvolution(stencilLeft.getBasicStencilAccess.stencil, stencilRight.getBasicStencilAccess.stencil)
  }
}

case class StencilFieldStencilConvolution(var stencilLeft : L4_StencilFieldAccess, var stencilRight : L4_StencilAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = { out << stencilLeft << " * " << stencilRight }

  def progress : IR_StencilFieldStencilConvolution = {
    IR_StencilFieldStencilConvolution(stencilLeft.getBasicStencilFieldAccess, stencilRight.getBasicStencilAccess.stencil)
  }
}
