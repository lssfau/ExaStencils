package exastencils.communication.ir

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.fieldlike.ir.IR_FieldLike

/// IR_IV_AbstractCommBufferLike

trait IR_IV_AbstractCommBufferLike extends IR_InternalVariableLike with IR_HasMessageDirection with IR_Expression {
  def field : IR_FieldLike
  def send : Boolean
  def size : IR_Expression
  def neighIdx : IR_Expression
  def concurrencyId : Int
  def indexOfRefinedNeighbor : Option[IR_Expression]
  def fragmentIdx : IR_Expression
}

/// IR_IV_CommBufferLike

trait IR_IV_CommBufferLike extends IR_IV_AbstractCommBufferLike {
  def field : IR_FieldLike
  def send : Boolean
  def size : IR_Expression
  def neighIdx : IR_Expression
  def concurrencyId : Int
  def indexOfRefinedNeighbor : Option[IR_Expression]
  def fragmentIdx : IR_Expression

  def basePtr : IR_Expression
}
