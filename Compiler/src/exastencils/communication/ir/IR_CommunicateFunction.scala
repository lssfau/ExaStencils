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
import exastencils.communication.NeighborInfo
import exastencils.communication.RefinementCases
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.domain.ir._
import exastencils.field.ir.IR_Field
import exastencils.logger.Logger
import exastencils.timing.ir.IR_AutomaticTimingCategory

/// IR_CommunicateFunction

case class IR_CommunicateFunction(
    var name : String,
    var level : Int,
    var field : IR_Field,
    var slot : IR_Expression,
    var neighbors : ListBuffer[NeighborInfo],
    var begin : Boolean, var finish : Boolean,
    var dupLayerExch : Boolean, var dupLayerBegin : IR_ExpressionIndex, var dupLayerEnd : IR_ExpressionIndex,
    var ghostLayerExch : Boolean, var ghostLayerBegin : IR_ExpressionIndex, var ghostLayerEnd : IR_ExpressionIndex,
    var insideFragLoop : Boolean,
    var condition : Option[IR_Expression],
    var direction : String) extends IR_FutureLeveledFunctionWithTiming {

  override def automaticTimingCategory = IR_AutomaticTimingCategory.COMM

  override def prettyprint_decl() = prettyprint

  def numDimsGrid = field.layout.numDimsGrid
  def numDimsData = field.layout.numDimsData

  def resolveIndex(indexId : String, dim : Int) = field.layout.idxById(indexId, dim)

  /* generate pack infos with iteration spaces */

  def genRefinedIndicesWrapper[T <: IR_PackInfo](
      send : Boolean,
      refinementCase : RefinementCases.Access,
      curNeighbors : ListBuffer[NeighborInfo],
      genPackInfo : (NeighborInfo, Int, IR_Field, IR_ExpressionIndex, IR_ExpressionIndex) => T) = {

    // create pack infos for different cases
    def getIndexOfRefinedNeighbor(neighInfo: NeighborInfo) = {
      if (send) // send: pack info per receiving neighbor (e.g. 1 for F2C in 2D)
        neighInfo.recvNeighborsForRefinementCase(refinementCase)
      else // recv: pack info per sending neighbor (e.g. 2 for F2C in 2D)
        neighInfo.sendNeighborsForRefinementCase(refinementCase)
    }

    curNeighbors.flatMap(neigh =>
      getIndexOfRefinedNeighbor(neigh).map(refIdx =>
        genPackInfo(Duplicate(neigh), refIdx, field, dupLayerBegin, dupLayerEnd)))
  }

  // equal level

  def genIndicesDuplicateRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    curNeighbors.map(neigh => IR_PackInfoDuplicateRemoteSend(Duplicate(neigh), field, dupLayerBegin, dupLayerEnd))
  def genIndicesDuplicateRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    curNeighbors.map(neigh => IR_PackInfoDuplicateRemoteRecv(Duplicate(neigh), field, dupLayerBegin, dupLayerEnd))

  def genIndicesDuplicateLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    curNeighbors.map(neigh => IR_PackInfoDuplicateLocalSend(Duplicate(neigh), field, dupLayerBegin, dupLayerEnd))
  def genIndicesDuplicateLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    curNeighbors.map(neigh => IR_PackInfoDuplicateLocalRecv(Duplicate(neigh), field, dupLayerBegin, dupLayerEnd))

  def genIndicesGhostRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    curNeighbors.map(neigh => IR_PackInfoGhostRemoteSend(Duplicate(neigh), field, ghostLayerBegin, ghostLayerEnd))
  def genIndicesGhostRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    curNeighbors.map(neigh => IR_PackInfoGhostRemoteRecv(Duplicate(neigh), field, ghostLayerBegin, ghostLayerEnd))

  def genIndicesGhostLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    curNeighbors.map(neigh => IR_PackInfoGhostLocalSend(Duplicate(neigh), field, ghostLayerBegin, ghostLayerEnd))
  def genIndicesGhostLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    curNeighbors.map(neigh => IR_PackInfoGhostLocalRecv(Duplicate(neigh), field, ghostLayerBegin, ghostLayerEnd))

  // fine-to-coarse

  def genF2CIndicesWrapper[T <: IR_PackInfo](
      send : Boolean,
      curNeighbors : ListBuffer[NeighborInfo],
      genPackInfo : (NeighborInfo, Int, IR_Field, IR_ExpressionIndex, IR_ExpressionIndex) => T) = {

    genRefinedIndicesWrapper(send, RefinementCases.F2C, curNeighbors, genPackInfo)
  }

  def genF2CIndicesDuplicateRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genF2CIndicesWrapper(send = true, curNeighbors, IR_F2CPackInfoDuplicateRemoteSend)
  def genF2CIndicesDuplicateRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genF2CIndicesWrapper(send = false, curNeighbors, IR_F2CPackInfoDuplicateRemoteRecv)

  def genF2CIndicesDuplicateLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genF2CIndicesWrapper(send = true, curNeighbors, IR_F2CPackInfoDuplicateLocalSend)
  def genF2CIndicesDuplicateLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genF2CIndicesWrapper(send = false, curNeighbors, IR_F2CPackInfoDuplicateLocalRecv)

  def genF2CIndicesGhostRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genF2CIndicesWrapper(send = true, curNeighbors, IR_F2CPackInfoGhostRemoteSend)
  def genF2CIndicesGhostRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genF2CIndicesWrapper(send = false, curNeighbors, IR_F2CPackInfoGhostRemoteRecv)

  def genF2CIndicesGhostLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genF2CIndicesWrapper(send = true, curNeighbors, IR_F2CPackInfoGhostLocalSend)
  def genF2CIndicesGhostLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genF2CIndicesWrapper(send = false, curNeighbors, IR_F2CPackInfoGhostLocalRecv)

  // coarse-to-fine

  def genC2FIndicesWrapper[T <: IR_PackInfo](
      send : Boolean,
      curNeighbors : ListBuffer[NeighborInfo],
      genPackInfo : (NeighborInfo, Int, IR_Field, IR_ExpressionIndex, IR_ExpressionIndex) => T) = {

    genRefinedIndicesWrapper(send, RefinementCases.C2F, curNeighbors, genPackInfo)
  }

  def genC2FIndicesDuplicateRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genC2FIndicesWrapper(send = true, curNeighbors, IR_C2FPackInfoDuplicateRemoteSend)
  def genC2FIndicesDuplicateRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genC2FIndicesWrapper(send = false, curNeighbors, IR_C2FPackInfoDuplicateRemoteRecv)

  def genC2FIndicesDuplicateLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genC2FIndicesWrapper(send = true, curNeighbors, IR_C2FPackInfoDuplicateLocalSend)
  def genC2FIndicesDuplicateLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genC2FIndicesWrapper(send = false, curNeighbors, IR_C2FPackInfoDuplicateLocalRecv)

  def genC2FIndicesGhostRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genC2FIndicesWrapper(send = true, curNeighbors, IR_C2FPackInfoGhostRemoteSend)
  def genC2FIndicesGhostRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genC2FIndicesWrapper(send = false, curNeighbors, IR_C2FPackInfoGhostRemoteRecv)

  def genC2FIndicesGhostLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genC2FIndicesWrapper(send = true, curNeighbors, IR_C2FPackInfoGhostLocalSend)
  def genC2FIndicesGhostLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genC2FIndicesWrapper(send = false, curNeighbors, IR_C2FPackInfoGhostLocalRecv)

  /* generate communication statements */

  def genCommStatements(
      concurrencyId : Int,
      refinementCase : RefinementCases.Access,
      remoteSendPackInfos : ListBuffer[IR_RemotePackInfo], remoteRecvPackInfos : ListBuffer[IR_RemotePackInfo],
      localSendPackInfos : ListBuffer[IR_LocalPackInfo], localRecvPackInfos : ListBuffer[IR_LocalPackInfo]) : ListBuffer[IR_Statement] = {

    var body = ListBuffer[IR_Statement]()

    if (begin) {
      body += IR_RemoteCommunicationStart(field, Duplicate(slot), refinementCase, remoteSendPackInfos,
        start = true, end = false, concurrencyId, insideFragLoop, condition)
      body += IR_RemoteCommunicationFinish(field, Duplicate(slot), refinementCase, remoteRecvPackInfos,
        start = true, end = false, concurrencyId, insideFragLoop, condition)
      body += IR_LocalCommunicationStart(field, Duplicate(slot), refinementCase, localSendPackInfos, localRecvPackInfos,
        insideFragLoop, condition)
    }
    if (finish) {
      body += IR_RemoteCommunicationFinish(field, Duplicate(slot), refinementCase, remoteRecvPackInfos,
        start = false, end = true, concurrencyId, insideFragLoop, condition)
      body += IR_RemoteCommunicationStart(field, Duplicate(slot), refinementCase, remoteSendPackInfos,
        start = false, end = true, concurrencyId, insideFragLoop, condition)
      body += IR_LocalCommunicationFinish(field, Duplicate(slot), refinementCase, localSendPackInfos, localRecvPackInfos,
        insideFragLoop, condition)
    }

    body
  }

  // equal level

  def genDuplicateCommStatements(concurrencyId : Int, sendNeighbors : ListBuffer[NeighborInfo], recvNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_Statement] = {
    val ret = genCommStatements(concurrencyId, RefinementCases.EQUAL,
      genIndicesDuplicateRemoteSend(sendNeighbors), genIndicesDuplicateRemoteRecv(recvNeighbors),
      genIndicesDuplicateLocalSend(sendNeighbors), genIndicesDuplicateLocalRecv(recvNeighbors))

    if (Knowledge.refinement_enabled) {
      ret ++= genF2CDuplicateCommStatements(concurrencyId, sendNeighbors, recvNeighbors)
      ret ++= genC2FDuplicateCommStatements(concurrencyId, sendNeighbors, recvNeighbors)
    }

    ret
  }

  def genGhostCommStatements(concurrencyId : Int, curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_Statement] = {
    val ret = genCommStatements(concurrencyId, RefinementCases.EQUAL,
      genIndicesGhostRemoteSend(curNeighbors), genIndicesGhostRemoteRecv(curNeighbors),
      genIndicesGhostLocalSend(curNeighbors), genIndicesGhostLocalRecv(curNeighbors))

    if (Knowledge.refinement_enabled) {
      ret ++= genF2CGhostCommStatements(concurrencyId, curNeighbors)
      ret ++= genC2FGhostCommStatements(concurrencyId, curNeighbors)
    }

    ret
  }

  // fine-to-coarse

  def genF2CDuplicateCommStatements(concurrencyId : Int, sendNeighbors : ListBuffer[NeighborInfo], recvNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_Statement] = {
    genCommStatements(concurrencyId, RefinementCases.F2C,
      genF2CIndicesDuplicateRemoteSend(sendNeighbors), genF2CIndicesDuplicateRemoteRecv(recvNeighbors),
      genF2CIndicesDuplicateLocalSend(sendNeighbors), genF2CIndicesDuplicateLocalRecv(recvNeighbors))
  }

  def genF2CGhostCommStatements(concurrencyId : Int, curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_Statement] = {
    genCommStatements(concurrencyId, RefinementCases.F2C,
      genF2CIndicesGhostRemoteSend(curNeighbors), genF2CIndicesGhostRemoteRecv(curNeighbors),
      genF2CIndicesGhostLocalSend(curNeighbors), genF2CIndicesGhostLocalRecv(curNeighbors))
  }

  // coarse-to-fine
  def genC2FDuplicateCommStatements(concurrencyId : Int, sendNeighbors : ListBuffer[NeighborInfo], recvNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_Statement] = {
    genCommStatements(concurrencyId, RefinementCases.C2F,
      genC2FIndicesDuplicateRemoteSend(sendNeighbors), genC2FIndicesDuplicateRemoteRecv(recvNeighbors),
      genC2FIndicesDuplicateLocalSend(sendNeighbors), genC2FIndicesDuplicateLocalRecv(recvNeighbors))
  }

  def genC2FGhostCommStatements(concurrencyId : Int, curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_Statement] = {
    genCommStatements(concurrencyId, RefinementCases.C2F,
      genC2FIndicesGhostRemoteSend(curNeighbors), genC2FIndicesGhostRemoteRecv(curNeighbors),
      genC2FIndicesGhostLocalSend(curNeighbors), genC2FIndicesGhostLocalRecv(curNeighbors))
  }

  def compileTransformedDuplicateComm(sendNeighbors : ListBuffer[NeighborInfo], recvNeighbors : ListBuffer[NeighborInfo], concurrencyId : Int) : ListBuffer[IR_Statement] = {
    var commStmts = ListBuffer[IR_Statement]()

    val refinementCase = RefinementCases.EQUAL // TODO: refinement not supported here

    val domains = IR_DomainCollection.objects

    def fragId = IR_IV_FragmentId()

    if (begin) {
      for (d <- domains.indices) {
        for (neigh <- neighbors) {
          commStmts += IR_IfCondition(
            if (sendNeighbors.contains(neigh))
              IR_GreaterEqual(fragId, IR_IV_NeighFragId(d, neigh.index))
            else
              IR_Greater(fragId, IR_IV_NeighFragId(d, neigh.index)),
            IR_RemoteCommunicationStart(field, Duplicate(slot), refinementCase, genIndicesDuplicateRemoteSend(ListBuffer(neigh)),
              start = true, end = false, concurrencyId, insideFragLoop = true, condition)
          )
        }
      }

      for (d <- domains.indices) {
        for (neigh <- neighbors) {
          commStmts += IR_IfCondition(
            if (sendNeighbors.contains(neigh))
              IR_LowerEqual(fragId, IR_IV_NeighFragId(d, neigh.index))
            else
              IR_Lower(fragId, IR_IV_NeighFragId(d, neigh.index)),
            IR_RemoteCommunicationFinish(field, Duplicate(slot), refinementCase, genIndicesDuplicateRemoteRecv(ListBuffer(neigh)),
              start = true, end = false, concurrencyId, insideFragLoop = true, condition)
          )
        }
      }

      // generate all possible combinations of neighbors
      def recursiveLoop(start : Int, sendNeighs : ListBuffer[NeighborInfo]) : ListBuffer[IR_Statement] = {
        var localStmts = ListBuffer[IR_Statement]()
        // case this one does not exist
        val recvNeighs = neighbors filterNot (sendNeighs contains _)

        var andStmts = ListBuffer[IR_Expression]()
        for (n <- sendNeighs) {
          if (sendNeighbors contains n)
            andStmts += IR_LowerEqual(fragId, IR_IV_NeighFragId(0, n.index))
          else
            andStmts += IR_Lower(fragId, IR_IV_NeighFragId(0, n.index))
        }
        for (n <- recvNeighs) {
          if (recvNeighbors contains n)
            andStmts += IR_GreaterEqual(fragId, IR_IV_NeighFragId(0, n.index))
          else
            andStmts += IR_Greater(fragId, IR_IV_NeighFragId(0, n.index))
        }

        localStmts += IR_IfCondition(andStmts.reduce(IR_OrOr),
          IR_LocalCommunicationStart(field, Duplicate(slot), refinementCase, genIndicesDuplicateLocalSend(sendNeighs), genIndicesDuplicateLocalRecv(recvNeighs),
            insideFragLoop = true, condition)
        )

        for (idx <- start until neighbors.length) {
          // case this exists and maybe also others
          localStmts ++= recursiveLoop(idx + 1, sendNeighs :+ neighbors(idx))
        }

        localStmts
      }

      commStmts ++= recursiveLoop(0, ListBuffer())
    }
    if (finish) {
      for (d <- domains.indices) {
        for (neigh <- neighbors) {
          commStmts += IR_IfCondition(
            if (recvNeighbors.contains(neigh))
              IR_LowerEqual(fragId, IR_IV_NeighFragId(d, neigh.index))
            else
              IR_Lower(fragId, IR_IV_NeighFragId(d, neigh.index)),
            IR_RemoteCommunicationFinish(field, Duplicate(slot), refinementCase, genIndicesDuplicateRemoteRecv(ListBuffer(neigh)),
              start = false, end = true, concurrencyId, insideFragLoop = true, condition)
          )
        }
      }

      for (d <- domains.indices) {
        for (neigh <- neighbors) {
          commStmts += IR_IfCondition(
            if (sendNeighbors.contains(neigh))
              IR_GreaterEqual(fragId, IR_IV_NeighFragId(d, neigh.index))
            else
              IR_Greater(fragId, IR_IV_NeighFragId(d, neigh.index)),
            IR_RemoteCommunicationStart(field, Duplicate(slot), refinementCase, genIndicesDuplicateRemoteSend(ListBuffer(neigh)),
              start = false, end = true, concurrencyId, insideFragLoop = true, condition)
          )
        }
      }

      // generate all possible combinations of neighbors
      def recursiveLoop(start : Int, sendNeighs : ListBuffer[NeighborInfo]) : ListBuffer[IR_Statement] = {
        var localStmts = ListBuffer[IR_Statement]()
        // case this one does not exist
        val recvNeighs = neighbors filterNot (sendNeighs contains _)

        var andStmts = ListBuffer[IR_Expression]()
        for (n <- sendNeighs) {
          if (sendNeighbors contains n)
            andStmts += IR_LowerEqual(fragId, IR_IV_NeighFragId(0, n.index))
          else
            andStmts += IR_Lower(fragId, IR_IV_NeighFragId(0, n.index))
        }
        for (n <- recvNeighs) {
          if (recvNeighbors contains n)
            andStmts += IR_GreaterEqual(fragId, IR_IV_NeighFragId(0, n.index))
          else
            andStmts += IR_Greater(fragId, IR_IV_NeighFragId(0, n.index))
        }

        localStmts += IR_IfCondition(andStmts.reduce(IR_OrOr),
          IR_LocalCommunicationFinish(field, Duplicate(slot), refinementCase, genIndicesDuplicateLocalSend(sendNeighs), genIndicesDuplicateLocalRecv(recvNeighs),
            insideFragLoop = true, condition)
        )

        for (idx <- start until neighbors.length) {
          // case this exists and maybe also others
          localStmts ++= recursiveLoop(idx + 1, sendNeighs :+ neighbors(idx))
        }

        localStmts
      }

      commStmts ++= recursiveLoop(0, ListBuffer())
    }
    commStmts
  }

  def compileBody() : ListBuffer[IR_Statement] = {
    var body = new ListBuffer[IR_Statement]

    // sync duplicate values
    if (dupLayerExch && field.communicatesDuplicated) {
      val concurrencyId = if (begin && finish) 0 else 0
      if (field.layout.layoutsPerDim.foldLeft(0)((old : Int, l) => old max l.numDupLayersLeft max l.numDupLayersRight) > 0) {
        if (Knowledge.comm_batchCommunication) {
          for (dim <- 0 until field.layout.numDimsGrid) {
            val sendNeighbors = direction match {
              case "" | "upstream" => ListBuffer(neighbors(2 * dim + 1))
              case "downstream"    => ListBuffer(neighbors(2 * dim + 0))
              case _               => Logger.error(s"Unsupported communication direction $direction")
            }
            val recvNeighbors = direction match {
              case "" | "upstream" => ListBuffer(neighbors(2 * dim + 0))
              case "downstream"    => ListBuffer(neighbors(2 * dim + 1))
              case _               => Logger.error(s"Unsupported communication direction $direction")
            }

            body ++= genDuplicateCommStatements(concurrencyId, sendNeighbors, recvNeighbors)
          }
        } else {
          val sendNeighbors = direction match {
            case "" | "upstream" => neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0)
            case "downstream"    => neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0)
            case _               => Logger.error(s"Unsupported communication direction $direction")
          }
          val recvNeighbors = direction match {
            case "" | "upstream" => neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0)
            case "downstream"    => neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0)
            case _               => Logger.error(s"Unsupported communication direction $direction")
          }

          if (Knowledge.comm_enableCommTransformations) {
            body += IR_LoopOverFragments(compileTransformedDuplicateComm(sendNeighbors, recvNeighbors, concurrencyId))
          } else {
            body ++= genDuplicateCommStatements(concurrencyId, sendNeighbors, recvNeighbors)
          }
        }
      }
    }

    // update ghost layers
    if (ghostLayerExch && field.communicatesGhosts) {
      val concurrencyId = if (begin && finish) 0 else 1
      if (field.layout.layoutsPerDim.foldLeft(0)((old : Int, l) => old max l.numGhostLayersLeft max l.numGhostLayersRight) > 0) {
        if (Knowledge.comm_batchCommunication) {
          for (dim <- 0 until field.layout.numDimsGrid) {
            val curNeighbors = ListBuffer(neighbors(2 * dim + 0), neighbors(2 * dim + 1))
            body ++= genGhostCommStatements(concurrencyId, curNeighbors)
          }
        } else {
          body ++= genGhostCommStatements(concurrencyId, neighbors)
        }
      }
    }

    body
  }

  override def generateFct() = {
    var fctArgs : ListBuffer[IR_FunctionArgument] = ListBuffer()
    fctArgs += IR_FunctionArgument("slot", IR_IntegerDatatype)
    if (insideFragLoop)
      fctArgs += IR_FunctionArgument(IR_LoopOverFragments.defIt)

    IR_LeveledFunction(name, level, IR_UnitDatatype, fctArgs, compileBody())
  }
}
