//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.optimization.ir

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.parallelization.ir.IR_ParallelizationInfo

object IR_Unrolling extends DefaultStrategy("Loop unrolling") {

  final val UNROLLED_ANNOT : String = "UNDone"

  private[optimization] final val startVar : String = "_start"
  private[optimization] final val intermVar : String = "_intermediate"
  private[optimization] final val endVar : String = "_end"

  private[optimization] def startVarAcc = IR_VariableAccess(startVar, IR_IntegerDatatype)
  private[optimization] def intermVarAcc = IR_VariableAccess(intermVar, IR_IntegerDatatype)
  private[optimization] def endVarAcc = IR_VariableAccess(endVar, IR_IntegerDatatype)

  private[optimization] def getBoundsDeclAndPostLoop(itVar : String, start : IR_Expression, endExcl : IR_Expression, oldIncr : Long,
      body : ListBuffer[IR_Statement], reduction : Option[IR_Reduction]) : (ListBuffer[IR_Statement], IR_Statement) = {

    def itVarAcc = IR_VariableAccess(itVar, IR_IntegerDatatype)

    val boundsDecls = new ListBuffer[IR_Statement]()
    boundsDecls += IR_VariableDeclaration(IR_IntegerDatatype, startVar, start)
    boundsDecls += IR_VariableDeclaration(IR_IntegerDatatype, endVar, endExcl)

    val postBegin = IR_VariableDeclaration(IR_IntegerDatatype, itVar, intermVarAcc)
    val postEnd = IR_Lower(itVarAcc, endVarAcc)
    val postIncr = IR_Assignment(itVarAcc, IR_IntegerConstant(oldIncr), "+=")

    // remainder loop must not be optimized => use a plain/default par info
    val postLoop = new IR_ForLoop(postBegin, postEnd, postIncr, body, IR_ParallelizationInfo())
    postLoop.parallelization.reduction = reduction

    (boundsDecls, postLoop)
  }

  private[optimization] def getIntermDecl(newIncr : Long) : IR_VariableDeclaration = {
    IR_VariableDeclaration(IR_IntegerDatatype, intermVar, getIntermExpr(newIncr))
  }

  def getIntermExpr(newIncr : Long) : IR_Expression = {
    IR_Maximum(startVarAcc, endVarAcc - ((endVarAcc - startVarAcc) Mod IR_IntegerConstant(newIncr)))
  }

  private[optimization] def addBounds(itVar : String, begin : IR_Statement, end : IR_Expression, incr : IR_Expression,
      writeDecls : Boolean, stmts : ListBuffer[IR_Statement]) : Boolean = {

    val (lower, isDecl) : (IR_Expression, Boolean) =
      begin match {
        case IR_Assignment(IR_VariableAccess(itVar2, IR_IntegerDatatype), init, "=") if itVar == itVar2 => (init, false)
        case IR_VariableDeclaration(IR_IntegerDatatype, itVar2, Some(init), _) if itVar == itVar2       => (init, true)

        case _ => throw UnrollException("cannot interpret loop begin: " + begin.prettyprint())
      }

    val upperExcl : IR_Expression =
      if (writeDecls) end match {
        case IR_Lower(IR_VariableAccess(itVar2, IR_IntegerDatatype), bound) if itVar == itVar2 =>
          bound

        case IR_LowerEqual(IR_VariableAccess(itVar2, IR_IntegerDatatype), bound) if itVar == itVar2 =>
          IR_Addition(bound, IR_IntegerConstant(1))

        case _ => throw UnrollException("cannot interpret loop end: " + end.prettyprint())
      }
      else null

    addBounds(lower, upperExcl, incr, writeDecls, stmts)

    isDecl
  }

  private[optimization] def addBounds(lower : IR_Expression, upperExcl : IR_Expression, incr : IR_Expression,
      writeDecls : Boolean, stmts : ListBuffer[IR_Statement]) : Unit = {

    val intermExpr = IR_Maximum(endVarAcc - ((endVarAcc - startVarAcc) Mod incr), startVarAcc)

    if (writeDecls) {
      stmts += IR_VariableDeclaration(IR_IntegerDatatype, startVar, lower)
      stmts += IR_VariableDeclaration(IR_IntegerDatatype, endVar, upperExcl)
      stmts += IR_VariableDeclaration(IR_IntegerDatatype, intermVar, intermExpr)
    } else
      stmts += IR_Assignment(intermVarAcc, intermExpr, "=")
  }

  this += new Transformation("optimize", UnrollInnermost, isParallel = true)
}

final case class UnrollException(msg : String) extends Exception(msg)

object UnrollInnermost extends PartialFunction[Node, Transformation.OutputType] {

  private final val DEBUG : Boolean = false
  private final val SKIP_ANNOT : String = "URSkip"

  def isDefinedAt(node : Node) : Boolean = {
    node match {
      case loop : IR_ForLoop =>
        loop.parallelization.isInnermost && loop.removeAnnotation(SKIP_ANNOT).isEmpty
      case _                 =>
        false
    }
  }

  def apply(node : Node) : Transformation.OutputType = {

    val loop = node.asInstanceOf[IR_ForLoop]

    var itVar : String = null
    def itVarAcc = IR_VariableAccess(itVar, IR_IntegerDatatype)
    var start : IR_Expression = null
    var endExcl : IR_Expression = null
    var oldStride : Long = 0
    var newStride : Long = 0
    try {
      val (itVar_, start_, endExcl_, oldStride_) = extractBoundsAndIncrement(loop.begin, loop.end, loop.inc)
      itVar = itVar_
      start = start_
      endExcl = endExcl_
      oldStride = oldStride_
      newStride = oldStride * Knowledge.opt_unroll
    } catch {
      case UnrollException(msg) =>
        if (DEBUG)
          println("[unroll]  unable to unroll loop: " + msg) // print directly, logger may be silenced by any surrounding strategy
        return node
    }

    val oldBody = Duplicate(loop.body) // duplicate for later use in post loop
    loop.begin = IR_VariableDeclaration(IR_IntegerDatatype, itVar, IR_Unrolling.startVarAcc)
    loop.end = IR_Lower(itVarAcc, IR_Unrolling.intermVarAcc)
    loop.inc = IR_Assignment(itVarAcc, IR_IntegerConstant(newStride), "+=")
    // duplicate private vars would also be possible...
    val interleave : Boolean = Knowledge.opt_unroll_interleave && loop.parallelization.potentiallyParallel && loop.parallelization.privateVars.isEmpty
    loop.body = duplicateStmts(loop.body, Knowledge.opt_unroll, itVar, oldStride, interleave)

    val annot = loop.removeAnnotation(IR_Unrolling.UNROLLED_ANNOT)
    val unrolled : Boolean = annot.isDefined
    var res : ListBuffer[IR_Statement] = null
    var intermDecl : IR_VariableDeclaration = null
    var postLoop : IR_Statement = null
    if (unrolled) {
      res = new ListBuffer[IR_Statement]()
      intermDecl = annot.get.asInstanceOf[IR_VariableDeclaration]
      intermDecl.initialValue = Some(IR_Unrolling.getIntermExpr(newStride))
    } else {
      val (boundsDecls, postLoop_) : (ListBuffer[IR_Statement], IR_Statement) =
        IR_Unrolling.getBoundsDeclAndPostLoop(itVar, start, endExcl, oldStride, oldBody, Duplicate(loop.parallelization.reduction))
      postLoop = postLoop_
      intermDecl = IR_Unrolling.getIntermDecl(newStride)
      res = boundsDecls += intermDecl
    }
    res += loop
    if (!unrolled)
      res += postLoop

    loop.annotate(SKIP_ANNOT)
    loop.annotate(IR_Unrolling.UNROLLED_ANNOT, intermDecl)
    if (unrolled)
      res
    else
      IR_Scope(res)
  }

  def extractBoundsAndIncrement(begin : IR_Statement, end : IR_Expression, inc : IR_Statement) : (String, IR_Expression, IR_Expression, Long) = {

    val (itVar, stride) = inc match {
      case IR_ExpressionStatement(IR_PreIncrement(IR_VariableAccess(itVar, IR_IntegerDatatype)))       => (itVar, 1L)
      case IR_ExpressionStatement(IR_PostIncrement(IR_VariableAccess(itVar, IR_IntegerDatatype)))      => (itVar, 1L)
      case IR_Assignment(IR_VariableAccess(itVar, IR_IntegerDatatype), IR_IntegerConstant(incr), "+=") => (itVar, incr)

      case IR_Assignment(IR_VariableAccess(itVar, IR_IntegerDatatype),
      IR_Addition(ListBuffer(IR_VariableAccess(itVar2, IR_IntegerDatatype), IR_IntegerConstant(incr))),
      "=") if itVar == itVar2 =>
        (itVar, incr)

      case IR_Assignment(IR_VariableAccess(itVar, IR_IntegerDatatype),
      IR_Addition(ListBuffer(IR_IntegerConstant(incr), IR_VariableAccess(itVar2, IR_IntegerDatatype))),
      "=") if itVar == itVar2 =>
        (itVar, incr)

      case _ => throw UnrollException("cannot determine stride or it is negative:  " + inc.prettyprint())
    }
    if (stride <= 0)
      throw UnrollException("loop stride must be positive:  " + inc.prettyprint())

    val lower : IR_Expression =
      begin match {
        case IR_VariableDeclaration(IR_IntegerDatatype, itVar2, Some(init), _) if itVar == itVar2       => init
        case IR_Assignment(IR_VariableAccess(itVar2, IR_IntegerDatatype), init, "=") if itVar == itVar2 => init
        case _                                                                                          => throw UnrollException("cannot interpret loop begin: " + begin.prettyprint())
      }

    val upperExcl : IR_Expression =
      end match {
        case IR_Lower(IR_VariableAccess(itVar2, IR_IntegerDatatype), bound) if itVar == itVar2      => bound
        case IR_LowerEqual(IR_VariableAccess(itVar2, IR_IntegerDatatype), bound) if itVar == itVar2 => IR_Addition(bound, IR_IntegerConstant(1))
        case _                                                                                      => throw UnrollException("cannot interpret loop end: " + end.prettyprint())
      }

    (itVar, lower, upperExcl, stride)
  }

  private def duplicateStmts(body : ListBuffer[IR_Statement], unrollFactor : Int,
      itVar : String, oldInc : Long, interleave : Boolean) : ListBuffer[IR_Statement] = {

    var njuBody : ListBuffer[IR_Statement] = null
    val replaceStrat = new UpdateLoopVarAndNames(itVar)
    val dups = new ListBuffer[Iterator[IR_Statement]]()

    for (i <- 1L until unrollFactor) {
      val dup = Duplicate(body)
      replaceStrat.offset = i * oldInc
      replaceStrat.applyStandalone(dup)
      IR_SimplifyExpression.SimplifyIndices.applyStandalone(dup)
      dups += dup.iterator.filter(s => !s.isInstanceOf[IR_Comment])
    }

    if (interleave) {
      njuBody = new ListBuffer[IR_Statement]()
      for (stmt <- body) {
        njuBody += stmt // reuse original statement
        if (!stmt.isInstanceOf[IR_Comment])
          for (iter <- dups)
            njuBody += iter.next()
      }
    } else {
      njuBody = body // reuse original statements
      for (dup <- dups)
        njuBody ++= dup
    }

    njuBody
  }

  class UpdateLoopVarAndNames(itVar : String)
    extends QuietDefaultStrategy("Add loop var offset, rename declarations, and/or add annotation") {

    private final val SKIP_ANNOT = "UpLVSkip"
    private val rename = new HashSet[String]()

    var offset : Long = 0

    this += new Transformation("apply", {
      case vAcc @ IR_VariableAccess(v, _) if v == itVar =>
        if (offset != 0 && vAcc.removeAnnotation(SKIP_ANNOT).isEmpty) {
          vAcc.annotate(SKIP_ANNOT) // already done
          vAcc.datatype = IR_IntegerDatatype // fix type, if required
          IR_Addition(vAcc, IR_IntegerConstant(offset))
        } else
          vAcc

      case loop @ IR_ForLoop(decl : IR_VariableDeclaration, _, _, _, _) if offset > 0 => // HACK: declaration must be handled first
        rename.add(decl.name)
        loop

      case vAcc : IR_VariableAccess if rename.contains(vAcc.name) =>
        vAcc.name += "_" + offset
        vAcc

      case decl : IR_VariableDeclaration if offset > 0 =>
        rename.add(decl.name)
        decl.name += "_" + offset
        decl
    })
  }

}
