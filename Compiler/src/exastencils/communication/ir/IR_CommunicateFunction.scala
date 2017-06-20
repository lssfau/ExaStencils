package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.NeighborInfo
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.deprecated.ir.IR_FieldSelection

/// IR_CommunicateFunction

// FIXME: refactor - seriously

case class IR_CommunicateFunction(
    var name : String,
    var fieldSelection : IR_FieldSelection,
    var neighbors : ListBuffer[NeighborInfo],
    var begin : Boolean, var finish : Boolean,
    var dupLayerExch : Boolean, var dupLayerBegin : IR_ExpressionIndex, var dupLayerEnd : IR_ExpressionIndex,
    var ghostLayerExch : Boolean, var ghostLayerBegin : IR_ExpressionIndex, var ghostLayerEnd : IR_ExpressionIndex,
    var insideFragLoop : Boolean,
    var condition : Option[IR_Expression]) extends IR_FutureFunction {

  override def prettyprint_decl() = prettyprint

  def numDimsGrid = fieldSelection.field.fieldLayout.numDimsGrid
  def numDimsData = fieldSelection.field.fieldLayout.numDimsData

  def resolveIndex(indexId : String, dim : Int) = fieldSelection.field.fieldLayout.idxById(indexId, dim)

  def genIndicesDuplicateRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)] = {
    val indices = curNeighbors.map(neigh => (neigh, IR_ExpressionIndexRange(
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map {
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("DLB", i)
            case 26 => resolveIndex("IB", i)
          }
          case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerEnd(i)
          case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerBegin(i)
        }),
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map {
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("DRE", i)
            case 26 => resolveIndex("DRE", i)
          }
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
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DLB", i)
              case 26 => resolveIndex("IB", i)
            }
            case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerEnd(i)
            case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerBegin(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DRE", i)
              case 26 => resolveIndex("DRE", i)
            }
            case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerBegin(i)
            case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerEnd(i)
          })),
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DLB", i)
              case 26 => resolveIndex("IB", i)
            }
            case i if -neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerEnd(i)
            case i if -neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerBegin(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DRE", i)
              case 26 => resolveIndex("DRE", i)
            }
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
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DLB", i)
              case 26 => resolveIndex("IB", i)
            }
            case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerEnd(i)
            case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerBegin(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DRE", i)
              case 26 => resolveIndex("DRE", i)
            }
            case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerBegin(i)
            case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerEnd(i)
          })),
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DLB", i)
              case 26 => resolveIndex("IB", i)
            }
            case i if -neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerEnd(i)
            case i if -neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerBegin(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DRE", i)
              case 26 => resolveIndex("DRE", i)
            }
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
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("DLB", i)
            case 26 => resolveIndex("IB", i)
          }
          case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) - dupLayerEnd(i)
          case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) + dupLayerBegin(i)
        }),
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map {
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("DRE", i)
            case 26 => resolveIndex("DRE", i)
          }
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
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("GLB", i)
            case 26 => resolveIndex("DLB", i)
          }
          case i if neigh.dir(i) < 0  => resolveIndex("IB", i) + ghostLayerBegin(i)
          case i if neigh.dir(i) > 0  => resolveIndex("IE", i) - ghostLayerEnd(i)
        }),
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map {
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("GRE", i)
            case 26 => resolveIndex("DRE", i)
          }
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
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GLB", i)
              case 26 => resolveIndex("DLB", i)
            }
            case i if neigh.dir(i) < 0  => resolveIndex("IB", i) + ghostLayerBegin(i)
            case i if neigh.dir(i) > 0  => resolveIndex("IE", i) - ghostLayerEnd(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GRE", i)
              case 26 => resolveIndex("DRE", i)
            }
            case i if neigh.dir(i) < 0  => resolveIndex("IB", i) + ghostLayerEnd(i)
            case i if neigh.dir(i) > 0  => resolveIndex("IE", i) - ghostLayerBegin(i)
          })),
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GLB", i)
              case 26 => resolveIndex("DLB", i)
            }
            case i if -neigh.dir(i) < 0  => resolveIndex("GLE", i) - ghostLayerEnd(i)
            case i if -neigh.dir(i) > 0  => resolveIndex("GRB", i) + ghostLayerBegin(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GRE", i)
              case 26 => resolveIndex("DRE", i)
            }
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
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GLB", i)
              case 26 => resolveIndex("DLB", i)
            }
            case i if neigh.dir(i) < 0  => resolveIndex("GLE", i) - ghostLayerEnd(i)
            case i if neigh.dir(i) > 0  => resolveIndex("GRB", i) + ghostLayerBegin(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GRE", i)
              case 26 => resolveIndex("DRE", i)
            }
            case i if neigh.dir(i) < 0  => resolveIndex("GLE", i) - ghostLayerBegin(i)
            case i if neigh.dir(i) > 0  => resolveIndex("GRB", i) + ghostLayerEnd(i)
          })),
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GLB", i)
              case 26 => resolveIndex("DLB", i)
            }
            case i if -neigh.dir(i) < 0  => resolveIndex("IB", i) + ghostLayerBegin(i)
            case i if -neigh.dir(i) > 0  => resolveIndex("IE", i) - ghostLayerEnd(i)
          }),
        IR_ExpressionIndex(
          (0 until numDimsGrid).toArray.map {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GRE", i)
              case 26 => resolveIndex("DRE", i)
            }
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
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("GLB", i)
            case 26 => resolveIndex("DLB", i)
          }
          case i if neigh.dir(i) < 0  => resolveIndex("GLE", i) - ghostLayerEnd(i)
          case i if neigh.dir(i) > 0  => resolveIndex("GRB", i) + ghostLayerBegin(i)
        }),
      IR_ExpressionIndex(
        (0 until numDimsGrid).toArray.map {
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("GRE", i)
            case 26 => resolveIndex("DRE", i)
          }
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
        Knowledge.comm_strategyFragment match {
          case 6  =>
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
          case 26 =>
            val sendNeighbors = neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0)
            val recvNeighbors = neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0)
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

    // update ghost layers
    if (ghostLayerExch && field.communicatesGhosts) {
      val concurrencyId = if (begin && finish) 0 else 1
      if (field.fieldLayout.layoutsPerDim.foldLeft(0)((old : Int, l) => old max l.numGhostLayersLeft max l.numGhostLayersRight) > 0) {
        Knowledge.comm_strategyFragment match {
          case 6  =>
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
          case 26 =>
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

  override def generateFct() : IR_Function = {
    var fctArgs : ListBuffer[IR_FunctionArgument] = ListBuffer()
    fctArgs += IR_FunctionArgument("slot", IR_IntegerDatatype)
    if (insideFragLoop)
      fctArgs += IR_FunctionArgument(IR_LoopOverFragments.defIt)

    IR_Function(IR_UnitDatatype, name, fctArgs, compileBody(fieldSelection))
  }
}
