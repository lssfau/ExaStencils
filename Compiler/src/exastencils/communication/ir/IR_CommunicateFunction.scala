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
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.domain.ir._
import exastencils.field.ir.IR_Field
import exastencils.logger.Logger

/// IR_CommunicateFunction

// FIXME: refactor - seriously

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
    var direction : String) extends IR_FutureLeveledFunction {

  override def prettyprint_decl() = prettyprint

  def numDimsGrid = field.layout.numDimsGrid
  def numDimsData = field.layout.numDimsData

  def resolveIndex(indexId : String, dim : Int) = field.layout.idxById(indexId, dim)

  def genIndicesDuplicateRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)] = {
    // TODO: this only works for comm_onlyAxisNeighbors == false if coarse grid topology is regular, otherwise iteration spaces must be adapted
    val indices = curNeighbors.map(neigh => (neigh, IR_ExpressionIndexRange(
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map {
          case i if neigh.dir(i) == 0 =>
            if (Knowledge.comm_onlyAxisNeighbors)
              resolveIndex("DLB", i)
            else
              resolveIndex("IB", i)
          case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerEnd(i)
          case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerBegin(i)
        }),
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map {
          case i if neigh.dir(i) == 0 =>
            if (Knowledge.comm_onlyAxisNeighbors)
              resolveIndex("DRE", i)
            else
              resolveIndex("DRE", i)
          case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerBegin(i)
          case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerEnd(i)
        }))))

    // TODO: honor fieldSelection.arrayIndex
    for (dim <- numDimsGrid until numDimsData)
      indices.transform(old => (old._1, { old._2.begin.indices :+= IR_IntegerConstant(0); old._2.end.indices :+= resolveIndex("TOT", dim); old._2 }))

    indices
  }

  def genIndicesDuplicateLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)] = {
    val indices = curNeighbors.map(neigh => (neigh,
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if neigh.dir(i) == 0 =>
              if (Knowledge.comm_onlyAxisNeighbors)
                resolveIndex("DLB", i)
              else
                resolveIndex("IB", i)
            case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerEnd(i)
            case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerBegin(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if neigh.dir(i) == 0 =>
              if (Knowledge.comm_onlyAxisNeighbors)
                resolveIndex("DRE", i)
              else
                resolveIndex("DRE", i)
            case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerBegin(i)
            case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerEnd(i)
          })),
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 =>
              if (Knowledge.comm_onlyAxisNeighbors)
                resolveIndex("DLB", i)
              else
                resolveIndex("IB", i)
            case i if -neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerEnd(i)
            case i if -neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerBegin(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 =>
              if (Knowledge.comm_onlyAxisNeighbors)
                resolveIndex("DRE", i)
              else
                resolveIndex("DRE", i)
            case i if -neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerBegin(i)
            case i if -neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerEnd(i)
          }))))

    // TODO: honor fieldSelection.arrayIndex
    for (dim <- numDimsGrid until numDimsData)
      indices.transform(old => (old._1, { old._2.begin.indices :+= IR_IntegerConstant(0); old._2.end.indices :+= resolveIndex("TOT", dim); old._2 }, { old._3.begin.indices :+= IR_IntegerConstant(0); old._3.end.indices :+= resolveIndex("TOT", dim); old._3 }))

    indices
  }

  def genIndicesDuplicateLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)] = {
    val indices = curNeighbors.map(neigh => (neigh,
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if neigh.dir(i) == 0 =>
              if (Knowledge.comm_onlyAxisNeighbors)
                resolveIndex("DLB", i)
              else
                resolveIndex("IB", i)
            case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerEnd(i)
            case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerBegin(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if neigh.dir(i) == 0 =>
              if (Knowledge.comm_onlyAxisNeighbors)
                resolveIndex("DRE", i)
              else
                resolveIndex("DRE", i)
            case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerBegin(i)
            case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerEnd(i)
          })),
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 =>
              if (Knowledge.comm_onlyAxisNeighbors)
                resolveIndex("DLB", i)
              else resolveIndex("IB", i)
            case i if -neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerEnd(i)
            case i if -neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerBegin(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 =>
              if (Knowledge.comm_onlyAxisNeighbors)
                resolveIndex("DRE", i)
              else
                resolveIndex("DRE", i)
            case i if -neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerBegin(i)
            case i if -neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerEnd(i)
          }))))

    // TODO: honor fieldSelection.arrayIndex
    for (dim <- numDimsGrid until numDimsData)
      indices.transform(old => (old._1, { old._2.begin.indices :+= IR_IntegerConstant(0); old._2.end.indices :+= resolveIndex("TOT", dim); old._2 }, { old._3.begin.indices :+= IR_IntegerConstant(0); old._3.end.indices :+= resolveIndex("TOT", dim); old._3 }))

    indices
  }

  def genIndicesDuplicateRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)] = {
    val indices = curNeighbors.map(neigh => (neigh, IR_ExpressionIndexRange(
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map {
          case i if neigh.dir(i) == 0 =>
            if (Knowledge.comm_onlyAxisNeighbors)
              resolveIndex("DLB", i)
            else
              resolveIndex("IB", i)
          case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerEnd(i)
          case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerBegin(i)
        }),
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map {
          case i if neigh.dir(i) == 0 =>
            if (Knowledge.comm_onlyAxisNeighbors)
              resolveIndex("DRE", i)
            else
              resolveIndex("DRE", i)
          case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerBegin(i)
          case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerEnd(i)
        }))))

    // TODO: honor fieldSelection.arrayIndex
    for (dim <- numDimsGrid until numDimsData)
      indices.transform(old => (old._1, { old._2.begin.indices :+= IR_IntegerConstant(0); old._2.end.indices :+= resolveIndex("TOT", dim); old._2 }))

    indices
  }

  def genIndicesGhostRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)] = {
    val indices = curNeighbors.map(neigh => (neigh, IR_ExpressionIndexRange(
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map {
          case i if neigh.dir(i) == 0 =>
            if (Knowledge.comm_syncGhostData)
              resolveIndex("GLB", i)
            else
              resolveIndex("DLB", i)
          case i if neigh.dir(i) < 0  => resolveIndex("IB", i) + ghostLayerBegin(i)
          case i if neigh.dir(i) > 0  => resolveIndex("IE", i) - ghostLayerEnd(i)
        }),
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map {
          case i if neigh.dir(i) == 0 =>
            if (Knowledge.comm_syncGhostData)
              resolveIndex("GRE", i)
            else
              resolveIndex("DRE", i)
          case i if neigh.dir(i) < 0  => resolveIndex("IB", i) + ghostLayerEnd(i)
          case i if neigh.dir(i) > 0  => resolveIndex("IE", i) - ghostLayerBegin(i)
        }))))

    // TODO: honor fieldSelection.arrayIndex
    for (dim <- numDimsGrid until numDimsData)
      indices.transform(old => (old._1, { old._2.begin.indices :+= IR_IntegerConstant(0); old._2.end.indices :+= resolveIndex("TOT", dim); old._2 }))

    indices
  }

  def genIndicesGhostLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)] = {
    val indices = curNeighbors.map(neigh => (neigh,
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if neigh.dir(i) == 0 =>
              if (Knowledge.comm_syncGhostData)
                resolveIndex("GLB", i)
              else
                resolveIndex("DLB", i)
            case i if neigh.dir(i) < 0  => resolveIndex("IB", i) + ghostLayerBegin(i)
            case i if neigh.dir(i) > 0  => resolveIndex("IE", i) - ghostLayerEnd(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if neigh.dir(i) == 0 =>
              if (Knowledge.comm_syncGhostData)
                resolveIndex("GRE", i)
              else
                resolveIndex("DRE", i)
            case i if neigh.dir(i) < 0  => resolveIndex("IB", i) + ghostLayerEnd(i)
            case i if neigh.dir(i) > 0  => resolveIndex("IE", i) - ghostLayerBegin(i)
          })),
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 =>
              if (Knowledge.comm_syncGhostData)
                resolveIndex("GLB", i)
              else
                resolveIndex("DLB", i)
            case i if -neigh.dir(i) < 0  => resolveIndex("GLE", i) - ghostLayerEnd(i)
            case i if -neigh.dir(i) > 0  => resolveIndex("GRB", i) + ghostLayerBegin(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 =>
              if (Knowledge.comm_syncGhostData)
                resolveIndex("GRE", i)
              else
                resolveIndex("DRE", i)
            case i if -neigh.dir(i) < 0  => resolveIndex("GLE", i) - ghostLayerBegin(i)
            case i if -neigh.dir(i) > 0  => resolveIndex("GRB", i) + ghostLayerEnd(i)
          }))))

    // TODO: honor fieldSelection.arrayIndex
    for (dim <- numDimsGrid until numDimsData)
      indices.transform(old => (old._1, { old._2.begin.indices :+= IR_IntegerConstant(0); old._2.end.indices :+= resolveIndex("TOT", dim); old._2 }, { old._3.begin.indices :+= IR_IntegerConstant(0); old._3.end.indices :+= resolveIndex("TOT", dim); old._3 }))

    indices
  }

  def genIndicesGhostLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)] = {
    val indices = curNeighbors.map(neigh => (neigh,
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if neigh.dir(i) == 0 =>
              if (Knowledge.comm_syncGhostData)
                resolveIndex("GLB", i)
              else
                resolveIndex("DLB", i)
            case i if neigh.dir(i) < 0  => resolveIndex("GLE", i) - ghostLayerEnd(i)
            case i if neigh.dir(i) > 0  => resolveIndex("GRB", i) + ghostLayerBegin(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if neigh.dir(i) == 0 =>
              if (Knowledge.comm_syncGhostData)
                resolveIndex("GRE", i)
              else
                resolveIndex("DRE", i)
            case i if neigh.dir(i) < 0  => resolveIndex("GLE", i) - ghostLayerBegin(i)
            case i if neigh.dir(i) > 0  => resolveIndex("GRB", i) + ghostLayerEnd(i)
          })),
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 =>
              if (Knowledge.comm_syncGhostData)
                resolveIndex("GLB", i)
              else
                resolveIndex("DLB", i)
            case i if -neigh.dir(i) < 0  => resolveIndex("IB", i) + ghostLayerBegin(i)
            case i if -neigh.dir(i) > 0  => resolveIndex("IE", i) - ghostLayerEnd(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 =>
              if (Knowledge.comm_syncGhostData)
                resolveIndex("GRE", i)
              else
                resolveIndex("DRE", i)
            case i if -neigh.dir(i) < 0  => resolveIndex("IB", i) + ghostLayerEnd(i)
            case i if -neigh.dir(i) > 0  => resolveIndex("IE", i) - ghostLayerBegin(i)
          }))))

    // TODO: honor fieldSelection.arrayIndex
    for (dim <- numDimsGrid until numDimsData)
      indices.transform(old => (old._1, { old._2.begin.indices :+= IR_IntegerConstant(0); old._2.end.indices :+= resolveIndex("TOT", dim); old._2 }, { old._3.begin.indices :+= IR_IntegerConstant(0); old._3.end.indices :+= resolveIndex("TOT", dim); old._3 }))

    indices
  }

  def genIndicesGhostRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)] = {
    val indices = curNeighbors.map(neigh => (neigh, IR_ExpressionIndexRange(
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map {
          case i if neigh.dir(i) == 0 =>
            if (Knowledge.comm_syncGhostData)
              resolveIndex("GLB", i)
            else
              resolveIndex("DLB", i)
          case i if neigh.dir(i) < 0  => resolveIndex("GLE", i) - ghostLayerEnd(i)
          case i if neigh.dir(i) > 0  => resolveIndex("GRB", i) + ghostLayerBegin(i)
        }),
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map {
          case i if neigh.dir(i) == 0 =>
            if (Knowledge.comm_syncGhostData)
              resolveIndex("GRE", i)
            else
              resolveIndex("DRE", i)
          case i if neigh.dir(i) < 0  => resolveIndex("GLE", i) - ghostLayerBegin(i)
          case i if neigh.dir(i) > 0  => resolveIndex("GRB", i) + ghostLayerEnd(i)
        }))))

    // TODO: honor fieldSelection.arrayIndex
    for (dim <- numDimsGrid until numDimsData)
      indices.transform(old => (old._1, { old._2.begin.indices :+= IR_IntegerConstant(0); old._2.end.indices :+= resolveIndex("TOT", dim); old._2 }))

    indices
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

            if (begin) {
              body += IR_RemoteCommunicationStart(field, Duplicate(slot), genIndicesDuplicateRemoteSend(sendNeighbors), true, false, concurrencyId, insideFragLoop, condition)
              body += IR_RemoteCommunicationFinish(field, Duplicate(slot), genIndicesDuplicateRemoteRecv(recvNeighbors), true, false, concurrencyId, insideFragLoop, condition)
              body += IR_LocalCommunicationStart(field, Duplicate(slot), genIndicesDuplicateLocalSend(sendNeighbors), genIndicesDuplicateLocalRecv(recvNeighbors), insideFragLoop, condition)
            }
            if (finish) {
              body += IR_RemoteCommunicationFinish(field, Duplicate(slot), genIndicesDuplicateRemoteRecv(recvNeighbors), false, true, concurrencyId, insideFragLoop, condition)
              body += IR_RemoteCommunicationStart(field, Duplicate(slot), genIndicesDuplicateRemoteSend(sendNeighbors), false, true, concurrencyId, insideFragLoop, condition)
              body += IR_LocalCommunicationFinish(field, Duplicate(slot), genIndicesDuplicateLocalSend(sendNeighbors), genIndicesDuplicateLocalRecv(recvNeighbors), insideFragLoop, condition)
            }
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

            var commStmts = ListBuffer[IR_Statement]()

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
                    IR_RemoteCommunicationStart(field, Duplicate(slot), genIndicesDuplicateRemoteSend(ListBuffer(neigh)), true, false, concurrencyId, true, condition)
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
                    IR_RemoteCommunicationFinish(field, Duplicate(slot), genIndicesDuplicateRemoteRecv(ListBuffer(neigh)), true, false, concurrencyId, true, condition)
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
                  IR_LocalCommunicationStart(field, Duplicate(slot), genIndicesDuplicateLocalSend(sendNeighs), genIndicesDuplicateLocalRecv(recvNeighs), true, condition)
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
                    IR_RemoteCommunicationFinish(field, Duplicate(slot), genIndicesDuplicateRemoteRecv(ListBuffer(neigh)), false, true, concurrencyId, true, condition)
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
                    IR_RemoteCommunicationStart(field, Duplicate(slot), genIndicesDuplicateRemoteSend(ListBuffer(neigh)), false, true, concurrencyId, true, condition)
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
                  IR_LocalCommunicationFinish(field, Duplicate(slot), genIndicesDuplicateLocalSend(sendNeighs), genIndicesDuplicateLocalRecv(recvNeighs), true, condition)
                )

                for (idx <- start until neighbors.length) {
                  // case this exists and maybe also others
                  localStmts ++= recursiveLoop(idx + 1, sendNeighs :+ neighbors(idx))
                }

                localStmts
              }

              commStmts ++= recursiveLoop(0, ListBuffer())
            }

            body += IR_LoopOverFragments(commStmts)

          }
          else {
            if (begin) {
              body += IR_RemoteCommunicationStart(field, Duplicate(slot), genIndicesDuplicateRemoteSend(sendNeighbors), true, false, concurrencyId, insideFragLoop, condition)
              body += IR_RemoteCommunicationFinish(field, Duplicate(slot), genIndicesDuplicateRemoteRecv(recvNeighbors), true, false, concurrencyId, insideFragLoop, condition)
              body += IR_LocalCommunicationStart(field, Duplicate(slot), genIndicesDuplicateLocalSend(sendNeighbors), genIndicesDuplicateLocalRecv(recvNeighbors), insideFragLoop, condition)
            }
            if (finish) {
              body += IR_RemoteCommunicationFinish(field, Duplicate(slot), genIndicesDuplicateRemoteRecv(recvNeighbors), false, true, concurrencyId, insideFragLoop, condition)
              body += IR_RemoteCommunicationStart(field, Duplicate(slot), genIndicesDuplicateRemoteSend(sendNeighbors), false, true, concurrencyId, insideFragLoop, condition)
              body += IR_LocalCommunicationFinish(field, Duplicate(slot), genIndicesDuplicateLocalSend(sendNeighbors), genIndicesDuplicateLocalRecv(recvNeighbors), insideFragLoop, condition)
            }
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

            if (begin) {
              body += IR_RemoteCommunicationStart(field, Duplicate(slot), genIndicesGhostRemoteSend(curNeighbors), true, false, concurrencyId, insideFragLoop, condition)
              body += IR_RemoteCommunicationFinish(field, Duplicate(slot), genIndicesGhostRemoteRecv(curNeighbors), true, false, concurrencyId, insideFragLoop, condition)
              body += IR_LocalCommunicationStart(field, Duplicate(slot), genIndicesGhostLocalSend(curNeighbors), genIndicesGhostLocalRecv(curNeighbors), insideFragLoop, condition)
            }
            if (finish) {
              body += IR_RemoteCommunicationFinish(field, Duplicate(slot), genIndicesGhostRemoteRecv(curNeighbors), false, true, concurrencyId, insideFragLoop, condition)
              body += IR_RemoteCommunicationStart(field, Duplicate(slot), genIndicesGhostRemoteSend(curNeighbors), false, true, concurrencyId, insideFragLoop, condition)
              body += IR_LocalCommunicationFinish(field, Duplicate(slot), genIndicesGhostLocalSend(curNeighbors), genIndicesGhostLocalRecv(curNeighbors), insideFragLoop, condition)
            }
          }
        } else {
          if (begin) {
            body += IR_RemoteCommunicationStart(field, Duplicate(slot), genIndicesGhostRemoteSend(neighbors), true, false, concurrencyId, insideFragLoop, condition)
            body += IR_RemoteCommunicationFinish(field, Duplicate(slot), genIndicesGhostRemoteRecv(neighbors), true, false, concurrencyId, insideFragLoop, condition)
            body += IR_LocalCommunicationStart(field, Duplicate(slot), genIndicesGhostLocalSend(neighbors), genIndicesGhostLocalRecv(neighbors), insideFragLoop, condition)
          }
          if (finish) {
            body += IR_RemoteCommunicationFinish(field, Duplicate(slot), genIndicesGhostRemoteRecv(neighbors), false, true, concurrencyId, insideFragLoop, condition)
            body += IR_RemoteCommunicationStart(field, Duplicate(slot), genIndicesGhostRemoteSend(neighbors), false, true, concurrencyId, insideFragLoop, condition)
            body += IR_LocalCommunicationFinish(field, Duplicate(slot), genIndicesGhostLocalSend(neighbors), genIndicesGhostLocalRecv(neighbors), insideFragLoop, condition)
          }
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
