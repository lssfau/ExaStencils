package exastencils.baseExt.ir.IR_MatNodes

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_Statement
import exastencils.baseExt.ir.IR_CompiletimeMatOps
import exastencils.datastructures.Node
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

// node to mark/ wrap declarations to temporary variables of extraction strategy
object IR_InlineableDeclaration {
  def apply(datatype : IR_Datatype, name : String, initialValue : IR_Expression) = {
    new IR_InlineableDeclaration(datatype, name, initialValue)
  }
}

case class IR_InlineableDeclaration(
    datatype : IR_Datatype,
    name : String,
    initialValue : IR_Expression
) extends IR_Statement {
  def isInlineable() : Boolean = {
    (initialValue match {
      case inv : IR_IntermediateInv => IR_CompiletimeMatOps.getSize(inv.arg)._1 < 4 && !inv.resolveAtRuntime
      case det : IR_Determinant     => false //getSize(det.arg)._1 < 4 && !det.resolveAtRuntime
      case gs : IR_GetSlice         => false //!gs.resolveAtRuntime
      case _                        => false
    })
  }
  override def prettyprint(out : PpStream) : Unit = Logger.error("internal node not resolved")
}

// node to wrap statements with leftover extractable expressions
object IR_ExtractableStatement {
  def apply(inner : IR_Statement, nExtractables : Int) = {
    new IR_ExtractableStatement(inner, nExtractables)
  }
}

case class IR_ExtractableStatement(inner : Node, var nExtractables : Int) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = Logger.error("internal node not resolved")
}