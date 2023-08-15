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

package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.boundary.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.config._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir.IR_SlotAccess
import exastencils.logger._
import exastencils.parallelization.api.mpi.MPI_WaitForRequest
import exastencils.parallelization.api.omp.OMP_WaitForFlag
import exastencils.scheduling.SingleSchedulable
import exastencils.util.ir.IR_StackCollector

/// IR_SetupCommunication

// TODO: refactor
object IR_SetupCommunication extends DefaultStrategy("Set up communication") {

  case class NameAndLevel(name : String, level : Int)

  var commFunctions = IR_CommunicationFunctions()
  var addedFunctions : ListBuffer[NameAndLevel] = ListBuffer()

  var firstCall = true
  var condCounter = 0

  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  override def apply(node : Option[Node] = None) = {
    if (firstCall) {
      commFunctions = IR_CommunicationFunctions.get
      addedFunctions.clear

      if (Knowledge.mpi_enabled && Knowledge.domain_canHaveRemoteNeighs)
        commFunctions += MPI_WaitForRequest
      if (Knowledge.omp_enabled && Knowledge.domain_canHaveLocalNeighs)
        commFunctions += OMP_WaitForFlag
    }

    super.apply(node)

    firstCall = false
  }

  this += new Transformation("Adding and linking communication functions", {
    case loop : IR_LoopOverPoints if firstCall => // skip communication statements in loops -> to be handled after resolving loops
      loop

    case communicateStatement : IR_Communicate =>
      val numDims = communicateStatement.field.layout.numDimsData

      val level = communicateStatement.field.level

      var commDup = false
      var dupBegin = IR_ExpressionIndex(Array.fill(numDims)(0))
      var dupEnd = IR_ExpressionIndex(Array.fill(numDims)(0))
      var commGhost = false
      var ghostBegin = IR_ExpressionIndex(Array.fill(numDims)(0))
      var ghostEnd = IR_ExpressionIndex(Array.fill(numDims)(0))

      val cond = communicateStatement.condition
      val direction = communicateStatement.direction

      var insideFragLoop = collector.stack.map(_.isInstanceOf[IR_LoopOverFragments]).reduce(_ || _)
      if (insideFragLoop && !Knowledge.experimental_allowCommInFragLoops) {
        Logger.warn("Found communication inside fragment loop; this is currently unsupported")
        insideFragLoop = false
      }

      // TODO: currently assumes numXXXRight == numXXXLeft
      if (communicateStatement.targets.exists(t => "all" == t.target)) {
        val target = communicateStatement.targets.find(t => "all" == t.target).get
        commDup = true
        dupBegin = target.begin.getOrElse(IR_ExpressionIndex(Array.fill(numDims)(0)))
        dupEnd = target.end.getOrElse(IR_ExpressionIndex((0 until numDims).toArray.map(dim => communicateStatement.field.layout(dim).numDupLayersLeft)))
        commGhost = true
        ghostBegin = target.begin.getOrElse(IR_ExpressionIndex(Array.fill(numDims)(0)))
        ghostEnd = target.end.getOrElse(IR_ExpressionIndex((0 until numDims).toArray.map(dim => communicateStatement.field.layout(dim).numGhostLayersLeft)))
      }
      if (communicateStatement.targets.exists(t => "dup" == t.target)) {
        val target = communicateStatement.targets.find(t => "dup" == t.target).get
        commDup = true
        dupBegin = target.begin.getOrElse(IR_ExpressionIndex(Array.fill(numDims)(0)))
        dupEnd = target.end.getOrElse(IR_ExpressionIndex((0 until numDims).toArray.map(dim => communicateStatement.field.layout(dim).numDupLayersLeft)))
      }
      if (communicateStatement.targets.exists(t => "ghost" == t.target)) {
        val target = communicateStatement.targets.find(t => "ghost" == t.target).get
        commGhost = true
        ghostBegin = target.begin.getOrElse(IR_ExpressionIndex(Array.fill(numDims)(0)))
        ghostEnd = target.end.getOrElse(IR_ExpressionIndex((0 until numDims).toArray.map(dim => communicateStatement.field.layout(dim).numGhostLayersLeft)))
      }

      var functionName = communicateStatement.op match {
        case "begin"  => s"beginExch${ communicateStatement.field.name }_"
        case "finish" => s"finishExch${ communicateStatement.field.name }_"
        case "both"   => s"exch${ communicateStatement.field.name }_"
      }
      functionName += /*s"_${ communicateStatement.field.arrayIndex.getOrElse("a") }_" +*/
        communicateStatement.targets.map(t => s"${ t.target }_${
          val begin : IR_ExpressionIndex = t.begin.getOrElse(IR_ExpressionIndex(Array.fill(numDims)("a" : IR_Expression)))
          (0 until numDims).toArray.map(dim => begin(dim).prettyprint).mkString("_")
        }_${
          val end : IR_ExpressionIndex = t.end.getOrElse(IR_ExpressionIndex(Array.fill(numDims)("a" : IR_Expression)))
          (0 until numDims).toArray.map(dim => end(dim).prettyprint).mkString("_")
        }").mkString("_")

      if (direction != "")
        functionName += s"_$direction"

      if (insideFragLoop)
        functionName += "_ifl"

      if (cond.isDefined) {
        // TODO: summarize communicate statements with identical conditions (and targets ofc)
        functionName += s"_c$condCounter"
        condCounter += 1
      }

      if (!addedFunctions.contains(NameAndLevel(functionName, level))) {
        addedFunctions += NameAndLevel(functionName, level)
        commFunctions += IR_CommunicateFunction(functionName, level, communicateStatement.field,
          IR_VariableAccess("slot", IR_IntegerDatatype), DefaultNeighbors.neighbors,
          "begin" == communicateStatement.op || "both" == communicateStatement.op,
          "finish" == communicateStatement.op || "both" == communicateStatement.op,
          commDup, dupBegin, dupEnd,
          commGhost, ghostBegin, ghostEnd,
          insideFragLoop,
          cond, direction)
      }

      communicateStatement.slot match {
        case IR_SlotAccess(slot, _) if IR_LoopOverFragments.defIt == slot.fragmentIdx => slot.fragmentIdx = 0
        case _                                                                        =>
      }

      var fctArgs : ListBuffer[IR_Expression] = ListBuffer()
      fctArgs += communicateStatement.slot
      if (insideFragLoop)
        fctArgs += IR_LoopOverFragments.defIt

      IR_FunctionCall(IR_LeveledInternalFunctionReference(functionName, level, IR_UnitDatatype), fctArgs) : IR_Statement

    case applyBCsStatement : IR_ApplyBC =>
      var insideFragLoop = collector.stack.map(_.isInstanceOf[IR_LoopOverFragments]).reduce(_ || _)
      if (insideFragLoop && !Knowledge.experimental_allowCommInFragLoops) {
        Logger.warn("Found apply BCs inside fragment loop; this is currently unsupported")
        insideFragLoop = false
      }

      val level = applyBCsStatement.field.level

      var functionName = s"applyBCs${ applyBCsStatement.field.name }"
      if (insideFragLoop) functionName += "_ifl"

      if (!addedFunctions.contains(NameAndLevel(functionName, level))) {
        addedFunctions += NameAndLevel(functionName, level)
        commFunctions += IR_ApplyBCFunction(functionName, applyBCsStatement.field, "slot", IR_LoopOverFragments.defIt,
          DefaultNeighbors.neighbors, insideFragLoop)
      }

      applyBCsStatement.slot match {
        case IR_SlotAccess(slot, _) if IR_LoopOverFragments.defIt == slot.fragmentIdx => slot.fragmentIdx = 0
        case _                                                                        =>
      }

      var fctArgs : ListBuffer[IR_Expression] = ListBuffer()
      fctArgs += applyBCsStatement.slot
      if (insideFragLoop)
        fctArgs += IR_LoopOverFragments.defIt

      IR_FunctionCall(IR_LeveledInternalFunctionReference(functionName, level, IR_UnitDatatype), fctArgs) : IR_Statement
  }, false)
}

/// IR_SetupCommunicationWrapper

case class IR_SetupCommunicationWrapper(firstCall : Boolean) extends SingleSchedulable {
  override def apply(applyAtNode : Option[Node]) : Unit = {
    if (firstCall)
      IR_SetupCommunication.firstCall = true
    IR_SetupCommunication.apply(applyAtNode)
  }
}
