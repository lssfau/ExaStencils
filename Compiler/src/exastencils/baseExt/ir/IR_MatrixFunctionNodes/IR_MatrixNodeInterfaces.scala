package exastencils.baseExt.ir.IR_MatrixFunctionNodes

import exastencils.base.ir.IR_Expression
import exastencils.datastructures.Transformation

// nodes that have to be checked for inlining and argument availability
trait IR_ResolvableMNode extends IR_Expression {
  def isResolvable() : Boolean
  def resolve() : Transformation.OutputType
}

// nodes that have to be considered for inlining
trait IR_ExtractableMNode extends IR_Expression {
  def isExtractable() : Boolean
}

//TODO necessary?
trait IR_InlinableMNode extends IR_Expression {
  def isInlineable() : Boolean
}
