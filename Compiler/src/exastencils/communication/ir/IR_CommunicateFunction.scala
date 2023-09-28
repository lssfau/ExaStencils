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
import exastencils.domain.ir.RefinementCase
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.domain.ir._
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.logger.Logger
import exastencils.timing.ir._

/// IR_CommunicateFunction

case class IR_CommunicateFunction(
    var name : String,
    var level : Int,
    var field : IR_FieldLike,
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

  def genRefinedPackInfosWrapper[T <: IR_PackInfo](
      send : Boolean,
      duplicate : Boolean,
      refinementCase : RefinementCase.Access,
      curNeighbors : ListBuffer[NeighborInfo],
      genPackInfo : (NeighborInfo, IR_Expression, IR_FieldLike, IR_ExpressionIndex, IR_ExpressionIndex) => T) : ListBuffer[T] = {

    // create pack infos for different cases
    def getIndexOfRefinedNeighbor(neighInfo : NeighborInfo) : List[IR_Expression] = refinementCase match {
      case c : RefinementCase.Access if c == RefinementCase.EQUAL =>
        // no refined neighbor index required -> default to 0
        List(0)
      case c : RefinementCase.Access if c == RefinementCase.F2C   =>
        // F2C: the current fragment is finer than the neighbor
        if (send) {
          // send: pack info per receiving neighbor (i.e. coarse neighbor receives 1 buffer from this fragment)
          // -> use IV with index for corresponding (spatial) section in coarse block
          List(IR_RefinementIndexForCoarseNeighbor(neighInfo.index, field.domain.index))
        } else {
          // recv: pack info per sending neighbor (i.e. this fragment receives 1 buffer from the coarse neighbor)
          // no refined neighbor index required -> default to 0
          List(0)
        }
      case c : RefinementCase.Access if c == RefinementCase.C2F   =>
        // C2F: the current fragment is coarser than the neighbor
        if (send) {
          // send: pack info per receiving neighbor (i.e. in 2D, 2 fine blocks receive one buffer each from this fragment)
          (0 until Knowledge.refinement_maxFineNeighborsForCommAxis).map(IR_IntegerConstant(_)).toList
        } else {
          // recv: pack info per sending neighbor (i.e. in 2D, this fragment receives 1 buffer per fine neighbor)
          List(0)
        }
    }

    val layerBegin = if (duplicate) dupLayerBegin else ghostLayerBegin
    val layerEnd = if (duplicate) dupLayerEnd else ghostLayerEnd

    curNeighbors.flatMap(neigh =>
      getIndexOfRefinedNeighbor(neigh).map(refIdx =>
        genPackInfo(Duplicate(neigh), refIdx, field, layerBegin, layerEnd)))
  }

  // equal level

  def genPackInfosDuplicateRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    curNeighbors.map(neigh => IR_PackInfoDuplicateRemoteSend(Duplicate(neigh), field, dupLayerBegin, dupLayerEnd))
  def genPackInfosDuplicateRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    curNeighbors.map(neigh => IR_PackInfoDuplicateRemoteRecv(Duplicate(neigh), field, dupLayerBegin, dupLayerEnd))

  def genPackInfosDuplicateLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    curNeighbors.map(neigh => IR_PackInfoDuplicateLocalSend(Duplicate(neigh), field, dupLayerBegin, dupLayerEnd))
  def genPackInfosDuplicateLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    curNeighbors.map(neigh => IR_PackInfoDuplicateLocalRecv(Duplicate(neigh), field, dupLayerBegin, dupLayerEnd))

  def genPackInfosGhostRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    curNeighbors.map(neigh => IR_PackInfoGhostRemoteSend(Duplicate(neigh), field, ghostLayerBegin, ghostLayerEnd))
  def genPackInfosGhostRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    curNeighbors.map(neigh => IR_PackInfoGhostRemoteRecv(Duplicate(neigh), field, ghostLayerBegin, ghostLayerEnd))

  def genPackInfosGhostLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    curNeighbors.map(neigh => IR_PackInfoGhostLocalSend(Duplicate(neigh), field, ghostLayerBegin, ghostLayerEnd))
  def genPackInfosGhostLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    curNeighbors.map(neigh => IR_PackInfoGhostLocalRecv(Duplicate(neigh), field, ghostLayerBegin, ghostLayerEnd))

  // fine-to-coarse

  def genF2CPackInfosWrapper[T <: IR_PackInfo](
      send : Boolean,
      duplicate : Boolean,
      curNeighbors : ListBuffer[NeighborInfo],
      genPackInfo : (NeighborInfo, IR_Expression, IR_FieldLike, IR_ExpressionIndex, IR_ExpressionIndex) => T) : ListBuffer[T] = {

    genRefinedPackInfosWrapper(send, duplicate, RefinementCase.F2C, curNeighbors, genPackInfo)
  }

  def genF2CPackInfosDuplicateRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genF2CPackInfosWrapper(send = true, duplicate = true, curNeighbors, IR_F2CPackInfoDuplicateRemoteSend)
  def genF2CPackInfosDuplicateRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genF2CPackInfosWrapper(send = false, duplicate = true, curNeighbors, IR_F2CPackInfoDuplicateRemoteRecv)

  def genF2CPackInfosDuplicateLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genF2CPackInfosWrapper(send = true, duplicate = true, curNeighbors, IR_F2CPackInfoDuplicateLocalSend)
  def genF2CPackInfosDuplicateLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genF2CPackInfosWrapper(send = false, duplicate = true, curNeighbors, IR_F2CPackInfoDuplicateLocalRecv)

  def genF2CPackInfosGhostRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genF2CPackInfosWrapper(send = true, duplicate = false, curNeighbors, IR_F2CPackInfoGhostRemoteSend)
  def genF2CPackInfosGhostRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genF2CPackInfosWrapper(send = false, duplicate = false, curNeighbors, IR_F2CPackInfoGhostRemoteRecv)

  def genF2CPackInfosGhostLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genF2CPackInfosWrapper(send = true, duplicate = false, curNeighbors, IR_F2CPackInfoGhostLocalSend)
  def genF2CPackInfosGhostLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genF2CPackInfosWrapper(send = false, duplicate = false, curNeighbors, IR_F2CPackInfoGhostLocalRecv)

  // coarse-to-fine

  def genC2FPackInfosWrapper[T <: IR_PackInfo](
      send : Boolean,
      duplicate : Boolean,
      curNeighbors : ListBuffer[NeighborInfo],
      genPackInfo : (NeighborInfo, IR_Expression, IR_FieldLike, IR_ExpressionIndex, IR_ExpressionIndex) => T) : ListBuffer[T] = {

    genRefinedPackInfosWrapper(send, duplicate, RefinementCase.C2F, curNeighbors, genPackInfo)
  }

  def genC2FPackInfosDuplicateRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genC2FPackInfosWrapper(send = true, duplicate = true, curNeighbors, IR_C2FPackInfoDuplicateRemoteSend)
  def genC2FPackInfosDuplicateRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genC2FPackInfosWrapper(send = false, duplicate = true, curNeighbors, IR_C2FPackInfoDuplicateRemoteRecv)

  def genC2FPackInfosDuplicateLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genC2FPackInfosWrapper(send = true, duplicate = true, curNeighbors, IR_C2FPackInfoDuplicateLocalSend)
  def genC2FPackInfosDuplicateLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genC2FPackInfosWrapper(send = false, duplicate = true, curNeighbors, IR_C2FPackInfoDuplicateLocalRecv)

  def genC2FPackInfosGhostRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genC2FPackInfosWrapper(send = true, duplicate = false, curNeighbors, IR_C2FPackInfoGhostRemoteSend)
  def genC2FPackInfosGhostRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_RemotePackInfo] =
    genC2FPackInfosWrapper(send = false, duplicate = false, curNeighbors, IR_C2FPackInfoGhostRemoteRecv)

  def genC2FPackInfosGhostLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genC2FPackInfosWrapper(send = true, duplicate = false, curNeighbors, IR_C2FPackInfoGhostLocalSend)
  def genC2FPackInfosGhostLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_LocalPackInfo] =
    genC2FPackInfosWrapper(send = false, duplicate = false, curNeighbors, IR_C2FPackInfoGhostLocalRecv)

  /* generate communication statements */

  def genCommStatements(
      concurrencyId : Int,
      refinementCase : RefinementCase.Access,
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
    val ret = genCommStatements(concurrencyId, RefinementCase.EQUAL,
      genPackInfosDuplicateRemoteSend(sendNeighbors), genPackInfosDuplicateRemoteRecv(recvNeighbors),
      genPackInfosDuplicateLocalSend(sendNeighbors), genPackInfosDuplicateLocalRecv(recvNeighbors))

    if (Knowledge.refinement_enabled) {
      ret ++= genF2CDuplicateCommStatements(concurrencyId, sendNeighbors, recvNeighbors)
      ret ++= genC2FDuplicateCommStatements(concurrencyId, sendNeighbors, recvNeighbors)
    }

    ret
  }

  def genGhostCommStatements(concurrencyId : Int, curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_Statement] = {
    val ret = genCommStatements(concurrencyId, RefinementCase.EQUAL,
      genPackInfosGhostRemoteSend(curNeighbors), genPackInfosGhostRemoteRecv(curNeighbors),
      genPackInfosGhostLocalSend(curNeighbors), genPackInfosGhostLocalRecv(curNeighbors))

    if (Knowledge.refinement_enabled) {
      ret ++= genF2CGhostCommStatements(concurrencyId, curNeighbors)
      ret ++= genC2FGhostCommStatements(concurrencyId, curNeighbors)
    }

    ret
  }

  // fine-to-coarse

  def genF2CDuplicateCommStatements(concurrencyId : Int, sendNeighbors : ListBuffer[NeighborInfo], recvNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_Statement] = {
    genCommStatements(concurrencyId, RefinementCase.F2C,
      genF2CPackInfosDuplicateRemoteSend(sendNeighbors), genF2CPackInfosDuplicateRemoteRecv(recvNeighbors),
      genF2CPackInfosDuplicateLocalSend(sendNeighbors), genF2CPackInfosDuplicateLocalRecv(recvNeighbors))
  }

  def genF2CGhostCommStatements(concurrencyId : Int, curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_Statement] = {
    genCommStatements(concurrencyId, RefinementCase.F2C,
      genF2CPackInfosGhostRemoteSend(curNeighbors), genF2CPackInfosGhostRemoteRecv(curNeighbors),
      genF2CPackInfosGhostLocalSend(curNeighbors), genF2CPackInfosGhostLocalRecv(curNeighbors))
  }

  // coarse-to-fine
  def genC2FDuplicateCommStatements(concurrencyId : Int, sendNeighbors : ListBuffer[NeighborInfo], recvNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_Statement] = {
    genCommStatements(concurrencyId, RefinementCase.C2F,
      genC2FPackInfosDuplicateRemoteSend(sendNeighbors), genC2FPackInfosDuplicateRemoteRecv(recvNeighbors),
      genC2FPackInfosDuplicateLocalSend(sendNeighbors), genC2FPackInfosDuplicateLocalRecv(recvNeighbors))
  }

  def genC2FGhostCommStatements(concurrencyId : Int, curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[IR_Statement] = {
    genCommStatements(concurrencyId, RefinementCase.C2F,
      genC2FPackInfosGhostRemoteSend(curNeighbors), genC2FPackInfosGhostRemoteRecv(curNeighbors),
      genC2FPackInfosGhostLocalSend(curNeighbors), genC2FPackInfosGhostLocalRecv(curNeighbors))
  }

  def compileTransformedDuplicateComm(sendNeighbors : ListBuffer[NeighborInfo], recvNeighbors : ListBuffer[NeighborInfo], concurrencyId : Int) : ListBuffer[IR_Statement] = {
    var commStmts = ListBuffer[IR_Statement]()

    val refinementCase = RefinementCase.EQUAL // TODO: refinement not supported here
    val indexOfRefinedNeighbor : Option[IR_Expression] = None

    val domains = IR_DomainCollection.objects

    def fragId = IR_IV_FragmentId()

    if (begin) {
      for (d <- domains.indices) {
        for (neigh <- neighbors) {
          commStmts += IR_IfCondition(
            if (sendNeighbors.contains(neigh))
              IR_GreaterEqual(fragId, IR_IV_NeighFragId(d, neigh.index, indexOfRefinedNeighbor))
            else
              IR_Greater(fragId, IR_IV_NeighFragId(d, neigh.index, indexOfRefinedNeighbor)),
            IR_RemoteCommunicationStart(field, Duplicate(slot), refinementCase, genPackInfosDuplicateRemoteSend(ListBuffer(neigh)),
              start = true, end = false, concurrencyId, insideFragLoop = true, condition)
          )
        }
      }

      for (d <- domains.indices) {
        for (neigh <- neighbors) {
          commStmts += IR_IfCondition(
            if (sendNeighbors.contains(neigh))
              IR_LowerEqual(fragId, IR_IV_NeighFragId(d, neigh.index, indexOfRefinedNeighbor))
            else
              IR_Lower(fragId, IR_IV_NeighFragId(d, neigh.index, indexOfRefinedNeighbor)),
            IR_RemoteCommunicationFinish(field, Duplicate(slot), refinementCase, genPackInfosDuplicateRemoteRecv(ListBuffer(neigh)),
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
            andStmts += IR_LowerEqual(fragId, IR_IV_NeighFragId(0, n.index, indexOfRefinedNeighbor))
          else
            andStmts += IR_Lower(fragId, IR_IV_NeighFragId(0, n.index, indexOfRefinedNeighbor))
        }
        for (n <- recvNeighs) {
          if (recvNeighbors contains n)
            andStmts += IR_GreaterEqual(fragId, IR_IV_NeighFragId(0, n.index, indexOfRefinedNeighbor))
          else
            andStmts += IR_Greater(fragId, IR_IV_NeighFragId(0, n.index, indexOfRefinedNeighbor))
        }

        localStmts += IR_IfCondition(andStmts.reduce(IR_OrOr),
          IR_LocalCommunicationStart(field, Duplicate(slot), refinementCase, genPackInfosDuplicateLocalSend(sendNeighs), genPackInfosDuplicateLocalRecv(recvNeighs),
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
              IR_LowerEqual(fragId, IR_IV_NeighFragId(d, neigh.index, indexOfRefinedNeighbor))
            else
              IR_Lower(fragId, IR_IV_NeighFragId(d, neigh.index, indexOfRefinedNeighbor)),
            IR_RemoteCommunicationFinish(field, Duplicate(slot), refinementCase, genPackInfosDuplicateRemoteRecv(ListBuffer(neigh)),
              start = false, end = true, concurrencyId, insideFragLoop = true, condition)
          )
        }
      }

      for (d <- domains.indices) {
        for (neigh <- neighbors) {
          commStmts += IR_IfCondition(
            if (sendNeighbors.contains(neigh))
              IR_GreaterEqual(fragId, IR_IV_NeighFragId(d, neigh.index, indexOfRefinedNeighbor))
            else
              IR_Greater(fragId, IR_IV_NeighFragId(d, neigh.index, indexOfRefinedNeighbor)),
            IR_RemoteCommunicationStart(field, Duplicate(slot), refinementCase, genPackInfosDuplicateRemoteSend(ListBuffer(neigh)),
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
            andStmts += IR_LowerEqual(fragId, IR_IV_NeighFragId(0, n.index, indexOfRefinedNeighbor))
          else
            andStmts += IR_Lower(fragId, IR_IV_NeighFragId(0, n.index, indexOfRefinedNeighbor))
        }
        for (n <- recvNeighs) {
          if (recvNeighbors contains n)
            andStmts += IR_GreaterEqual(fragId, IR_IV_NeighFragId(0, n.index, indexOfRefinedNeighbor))
          else
            andStmts += IR_Greater(fragId, IR_IV_NeighFragId(0, n.index, indexOfRefinedNeighbor))
        }

        localStmts += IR_IfCondition(andStmts.reduce(IR_OrOr),
          IR_LocalCommunicationFinish(field, Duplicate(slot), refinementCase, genPackInfosDuplicateLocalSend(sendNeighs), genPackInfosDuplicateLocalRecv(recvNeighs),
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
