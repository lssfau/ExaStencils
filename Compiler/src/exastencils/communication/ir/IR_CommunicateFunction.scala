package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.communication.NeighborInfo
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.domain.ir.IR_DomainCollection
import exastencils.domain.ir.IR_IV_FragmentId

/// IR_CommunicateFunction

// FIXME: refactor - seriously

case class IR_CommunicateFunction(
    var name : String,
    var level : Int,
    var fieldSelection : IR_FieldSelection,
    var neighbors : ListBuffer[NeighborInfo],
    var begin : Boolean, var finish : Boolean,
    var dupLayerExch : Boolean, var dupLayerBegin : IR_ExpressionIndex, var dupLayerEnd : IR_ExpressionIndex,
    var ghostLayerExch : Boolean, var ghostLayerBegin : IR_ExpressionIndex, var ghostLayerEnd : IR_ExpressionIndex,
    var insideFragLoop : Boolean,
    var condition : Option[IR_Expression]) extends IR_FutureLeveledFunction {

  override def prettyprint_decl() = prettyprint

  def numDimsGrid = fieldSelection.field.fieldLayout.numDimsGrid
  def numDimsData = fieldSelection.field.fieldLayout.numDimsData

  def resolveIndex(indexId : String, dim : Int) = fieldSelection.field.fieldLayout.idxById(indexId, dim)

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

  def compileBody(updatedFieldSelection : IR_FieldSelection) : ListBuffer[IR_Statement] = {
    var body = new ListBuffer[IR_Statement]

    def updatedFieldSelectionDup = Duplicate(updatedFieldSelection)

    val field = updatedFieldSelectionDup.field

    // sync duplicate values
    if (dupLayerExch && field.communicatesDuplicated) {
      val concurrencyId = if (begin && finish) 0 else 0
      if (field.fieldLayout.layoutsPerDim.foldLeft(0)((old : Int, l) => old max l.numDupLayersLeft max l.numDupLayersRight) > 0) {
        if (Knowledge.comm_batchCommunication) {
          for (dim <- 0 until field.fieldLayout.numDimsGrid) {
            val recvNeighbors = ListBuffer(neighbors(2 * dim + 0))
            val sendNeighbors = ListBuffer(neighbors(2 * dim + 1))

            if (begin) {
              body += IR_RemoteCommunicationStart(updatedFieldSelectionDup, genIndicesDuplicateRemoteSend(sendNeighbors), true, false, concurrencyId, insideFragLoop, condition)
              body += IR_RemoteCommunicationFinish(updatedFieldSelectionDup, genIndicesDuplicateRemoteRecv(recvNeighbors), true, false, concurrencyId, insideFragLoop, condition)
              body += IR_LocalCommunicationStart(updatedFieldSelectionDup, genIndicesDuplicateLocalSend(sendNeighbors), genIndicesDuplicateLocalRecv(recvNeighbors), insideFragLoop, condition)
            }
            if (finish) {
              body += IR_RemoteCommunicationFinish(updatedFieldSelectionDup, genIndicesDuplicateRemoteRecv(recvNeighbors), false, true, concurrencyId, insideFragLoop, condition)
              body += IR_RemoteCommunicationStart(updatedFieldSelectionDup, genIndicesDuplicateRemoteSend(sendNeighbors), false, true, concurrencyId, insideFragLoop, condition)
              body += IR_LocalCommunicationFinish(updatedFieldSelectionDup, genIndicesDuplicateLocalSend(sendNeighbors), genIndicesDuplicateLocalRecv(recvNeighbors), insideFragLoop, condition)
            }
          }
        } else {
          val sendNeighbors = neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0)
          val recvNeighbors = neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0)

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
                    IR_RemoteCommunicationStart(updatedFieldSelectionDup, genIndicesDuplicateRemoteSend(ListBuffer(neigh)), true, false, concurrencyId, true, condition)
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
                    IR_RemoteCommunicationFinish(updatedFieldSelectionDup, genIndicesDuplicateRemoteRecv(ListBuffer(neigh)), true, false, concurrencyId, true, condition)
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
                  IR_LocalCommunicationStart(updatedFieldSelectionDup, genIndicesDuplicateLocalSend(sendNeighs), genIndicesDuplicateLocalRecv(recvNeighs), true, condition)
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
                    IR_RemoteCommunicationFinish(updatedFieldSelectionDup, genIndicesDuplicateRemoteRecv(ListBuffer(neigh)), false, true, concurrencyId, true, condition)
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
                    IR_RemoteCommunicationStart(updatedFieldSelectionDup, genIndicesDuplicateRemoteSend(ListBuffer(neigh)), false, true, concurrencyId, true, condition)
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
                  IR_LocalCommunicationFinish(updatedFieldSelectionDup, genIndicesDuplicateLocalSend(sendNeighs), genIndicesDuplicateLocalRecv(recvNeighs), true, condition)
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
              body += IR_RemoteCommunicationStart(updatedFieldSelectionDup, genIndicesDuplicateRemoteSend(sendNeighbors), true, false, concurrencyId, insideFragLoop, condition)
              body += IR_RemoteCommunicationFinish(updatedFieldSelectionDup, genIndicesDuplicateRemoteRecv(recvNeighbors), true, false, concurrencyId, insideFragLoop, condition)
              body += IR_LocalCommunicationStart(updatedFieldSelectionDup, genIndicesDuplicateLocalSend(sendNeighbors), genIndicesDuplicateLocalRecv(recvNeighbors), insideFragLoop, condition)
            }
            if (finish) {
              body += IR_RemoteCommunicationFinish(updatedFieldSelectionDup, genIndicesDuplicateRemoteRecv(recvNeighbors), false, true, concurrencyId, insideFragLoop, condition)
              body += IR_RemoteCommunicationStart(updatedFieldSelectionDup, genIndicesDuplicateRemoteSend(sendNeighbors), false, true, concurrencyId, insideFragLoop, condition)
              body += IR_LocalCommunicationFinish(updatedFieldSelectionDup, genIndicesDuplicateLocalSend(sendNeighbors), genIndicesDuplicateLocalRecv(recvNeighbors), insideFragLoop, condition)
            }

          }

        }
      }
    }

    // update ghost layers
    if (ghostLayerExch && field.communicatesGhosts) {
      val concurrencyId = if (begin && finish) 0 else 1
      if (field.fieldLayout.layoutsPerDim.foldLeft(0)((old : Int, l) => old max l.numGhostLayersLeft max l.numGhostLayersRight) > 0) {
        if (Knowledge.comm_batchCommunication) {
          for (dim <- 0 until field.fieldLayout.numDimsGrid) {
            val curNeighbors = ListBuffer(neighbors(2 * dim + 0), neighbors(2 * dim + 1))

            if (begin) {
              body += IR_RemoteCommunicationStart(updatedFieldSelectionDup, genIndicesGhostRemoteSend(curNeighbors), true, false, concurrencyId, insideFragLoop, condition)
              body += IR_RemoteCommunicationFinish(updatedFieldSelectionDup, genIndicesGhostRemoteRecv(curNeighbors), true, false, concurrencyId, insideFragLoop, condition)
              body += IR_LocalCommunicationStart(updatedFieldSelectionDup, genIndicesGhostLocalSend(curNeighbors), genIndicesGhostLocalRecv(curNeighbors), insideFragLoop, condition)
            }
            if (finish) {
              body += IR_RemoteCommunicationFinish(updatedFieldSelectionDup, genIndicesGhostRemoteRecv(curNeighbors), false, true, concurrencyId, insideFragLoop, condition)
              body += IR_RemoteCommunicationStart(updatedFieldSelectionDup, genIndicesGhostRemoteSend(curNeighbors), false, true, concurrencyId, insideFragLoop, condition)
              body += IR_LocalCommunicationFinish(updatedFieldSelectionDup, genIndicesGhostLocalSend(curNeighbors), genIndicesGhostLocalRecv(curNeighbors), insideFragLoop, condition)
            }
          }
        } else {
          if (begin) {
            body += IR_RemoteCommunicationStart(updatedFieldSelectionDup, genIndicesGhostRemoteSend(neighbors), true, false, concurrencyId, insideFragLoop, condition)
            body += IR_RemoteCommunicationFinish(updatedFieldSelectionDup, genIndicesGhostRemoteRecv(neighbors), true, false, concurrencyId, insideFragLoop, condition)
            body += IR_LocalCommunicationStart(updatedFieldSelectionDup, genIndicesGhostLocalSend(neighbors), genIndicesGhostLocalRecv(neighbors), insideFragLoop, condition)
          }
          if (finish) {
            body += IR_RemoteCommunicationFinish(updatedFieldSelectionDup, genIndicesGhostRemoteRecv(neighbors), false, true, concurrencyId, insideFragLoop, condition)
            body += IR_RemoteCommunicationStart(updatedFieldSelectionDup, genIndicesGhostRemoteSend(neighbors), false, true, concurrencyId, insideFragLoop, condition)
            body += IR_LocalCommunicationFinish(updatedFieldSelectionDup, genIndicesGhostLocalSend(neighbors), genIndicesGhostLocalRecv(neighbors), insideFragLoop, condition)
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

    IR_LeveledFunction(name, level, IR_UnitDatatype, fctArgs, compileBody(fieldSelection))
  }
}
