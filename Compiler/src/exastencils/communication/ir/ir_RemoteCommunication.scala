package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir._
import exastencils.knowledge.NeighborInfo
import exastencils.omp.OMP_PotentiallyParallel

/// IR_RemoteCommunication

abstract class IR_RemoteCommunication extends IR_Statement with IR_Expandable {
  def field : IR_FieldSelection
  def neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)]

  def insideFragLoop : Boolean

  def genCopy(neighbor : NeighborInfo, indices : IR_ExpressionIndexRange, addCondition : Boolean) : IR_Statement
  def genTransfer(neighbor : NeighborInfo, indices : IR_ExpressionIndexRange, addCondition : Boolean) : IR_Statement

  def wrapCond(neighbor : NeighborInfo, body : ListBuffer[IR_Statement]) : IR_Statement = {
    IR_IfCondition(IR_IV_NeighborIsValid(field.domainIndex, neighbor.index) AndAnd IR_IV_NeighborIsRemote(field.domainIndex, neighbor.index),
      body)
  }

  def wrapFragLoop(toWrap : IR_Statement, parallel : Boolean) : IR_Statement = {
    if (insideFragLoop)
      toWrap
    else if (parallel) {
      val loop = new IR_LoopOverFragments(ListBuffer[IR_Statement](toWrap)) with OMP_PotentiallyParallel
      loop.parallelization.potentiallyParallel = true
      loop
    } else
      IR_LoopOverFragments(toWrap)
  }
}
