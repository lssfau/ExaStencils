package exastencils.baseExt.ir.IR_MatNodes

import exastencils.base.ir.IR_Expression
import exastencils.datastructures.Transformation

// nodes that can be resolved to results
trait IR_ResolvableMNode extends IR_Expression {
  def isResolvable() : Boolean
  def resolve() : Transformation.OutputType
}

// nodes that have to be considered for inlining
trait IR_ExtractableMNode extends IR_Expression {
  def isExtractable() : Boolean
}

// nodes that have to be considered for inlining and can later be resolved at runtime
trait IR_RuntimeMNode extends IR_ExtractableMNode {
  def name : String
  def resolveAtRuntime : Boolean
}

