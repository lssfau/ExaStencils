package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.communication._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.multiGrid._

// FIXME: Think about moving all of this index information to some other source. Maybe some kind of ... DSL ... or even Layer4

case class ExchangeDataFunction(var fieldSelection : FieldSelection,
    var neighbors : ListBuffer[NeighborInfo],
    var begin : Boolean,
    var finish : Boolean) extends AbstractFunctionStatement with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = ExchangeDataFunction\n"
  override def cpp_decl = cpp

  def genIndicesBoundaryHandling() : ListBuffer[(NeighborInfo, IndexRange)] = {
    // FIXME: this works for now, but might be adapted later to incorporate different regions of boundary handling
    neighbors.map(neigh => (neigh, new IndexRange(
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupLeftBegin //idxDupLeftBegin, idxGhostLeftBegin
          case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftBegin //idxDupLeftBegin, idxGhostLeftBegin
          case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightBegin
        }) ++ Array(0)),
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupRightEnd //idxDupRightEnd, idxGhostRightEnd
          case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftEnd
          case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightEnd //idxDupRightEnd, idxGhostRightEnd
        }) ++ Array(fieldSelection.field.vectorSize)))))
  }

  def genIndicesDuplicateRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange)] = {
    Knowledge.comm_strategyFragment match {
      case 6 => curNeighbors.map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupLeftBegin
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftBegin
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightBegin
          }) ++ Array(0)),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupRightEnd
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftEnd
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightEnd
          }) ++ Array(fieldSelection.field.vectorSize)))))
      case 26 => curNeighbors.map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxInnerBegin
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftBegin
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightBegin
          }) ++ Array(0)),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxInnerEnd
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftEnd
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightEnd
          }) ++ Array(fieldSelection.field.vectorSize)))))
    }
  }

  def genIndicesDuplicateLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange, IndexRange)] = {
    Knowledge.comm_strategyFragment match {
      case 6 => curNeighbors.map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupLeftBegin
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftBegin
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightBegin
          }) ++ Array(0)),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupRightEnd
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftEnd
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightEnd
          }) ++ Array(fieldSelection.field.vectorSize))),
        new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if -neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupLeftBegin
              case i if -neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftBegin
              case i if -neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightBegin
            }) ++ Array(0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if -neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupRightEnd
              case i if -neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftEnd
              case i if -neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightEnd
            }) ++ Array(fieldSelection.field.vectorSize)))))
      case 26 => curNeighbors.map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxInnerBegin
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftBegin
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightBegin
          }) ++ Array(0)),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxInnerEnd
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftEnd
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightEnd
          }) ++ Array(fieldSelection.field.vectorSize))),
        new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if -neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxInnerBegin
              case i if -neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftBegin
              case i if -neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightBegin
            }) ++ Array(0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if -neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxInnerEnd
              case i if -neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftEnd
              case i if -neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightEnd
            }) ++ Array(fieldSelection.field.vectorSize)))))
    }
  }

  def genIndicesDuplicateRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange)] = {
    Knowledge.comm_strategyFragment match {
      case 6 => curNeighbors.map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupLeftBegin
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftBegin
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightBegin
          }) ++ Array(0)),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupRightEnd
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftEnd
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightEnd
          }) ++ Array(fieldSelection.field.vectorSize)))))
      case 26 => curNeighbors.map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxInnerBegin
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftBegin
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightBegin
          }) ++ Array(0)),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxInnerEnd
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxDupLeftEnd
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxDupRightEnd
          }) ++ Array(fieldSelection.field.vectorSize)))))
    }
  }

  def genIndicesGhostRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange)] = {
    Knowledge.comm_strategyFragment match {
      case 6 => curNeighbors.map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxGhostLeftBegin
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxInnerBegin
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxInnerBegin + fieldSelection.field.layout(i).numInnerLayers - fieldSelection.field.layout(i).numGhostLayersRight
          }) ++ Array(0)),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxGhostRightEnd
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxInnerBegin + fieldSelection.field.layout(i).numGhostLayersLeft
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxInnerEnd
          }) ++ Array(fieldSelection.field.vectorSize)))))
      case 26 => curNeighbors.map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupLeftBegin
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxInnerBegin
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxInnerBegin + fieldSelection.field.layout(i).numInnerLayers - fieldSelection.field.layout(i).numGhostLayersRight
          }) ++ Array(0)),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupRightEnd
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxInnerBegin + fieldSelection.field.layout(i).numGhostLayersLeft
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxInnerEnd
          }) ++ Array(fieldSelection.field.vectorSize)))))
    }
  }

  def genIndicesGhostLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange, IndexRange)] = {
    Knowledge.comm_strategyFragment match {
      case 6 => curNeighbors.map(neigh => (neigh,
        new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxGhostLeftBegin
              case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxInnerBegin
              case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxInnerBegin + fieldSelection.field.layout(i).numInnerLayers - fieldSelection.field.layout(i).numGhostLayersRight
            }) ++ Array(0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxGhostRightEnd
              case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxInnerBegin + fieldSelection.field.layout(i).numGhostLayersLeft
              case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxInnerEnd
            }) ++ Array(fieldSelection.field.vectorSize))),
        new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if -neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxGhostLeftBegin
              case i if -neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxGhostLeftBegin
              case i if -neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxGhostRightBegin
            }) ++ Array(0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if -neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxGhostRightEnd
              case i if -neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxGhostLeftEnd
              case i if -neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxGhostRightEnd
            }) ++ Array(fieldSelection.field.vectorSize)))))
      case 26 => curNeighbors.map(neigh => (neigh,
        new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupLeftBegin
              case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxInnerBegin
              case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxInnerBegin + fieldSelection.field.layout(i).numInnerLayers - fieldSelection.field.layout(i).numGhostLayersRight
            }) ++ Array(0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupRightEnd
              case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxInnerBegin + fieldSelection.field.layout(i).numGhostLayersLeft
              case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxInnerEnd
            }) ++ Array(fieldSelection.field.vectorSize))),
        new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if -neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupLeftBegin
              case i if -neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxGhostLeftBegin
              case i if -neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxGhostRightBegin
            }) ++ Array(0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if -neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupRightEnd
              case i if -neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxGhostLeftEnd
              case i if -neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxGhostRightEnd
            }) ++ Array(fieldSelection.field.vectorSize)))))
    }
  }

  def genIndicesGhostRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange)] = {
    Knowledge.comm_strategyFragment match {
      case 6 => curNeighbors.map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxGhostLeftBegin
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxGhostLeftBegin
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxGhostRightBegin
          }) ++ Array(0)),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxGhostRightEnd
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxGhostLeftEnd
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxGhostRightEnd
          }) ++ Array(fieldSelection.field.vectorSize)))))
      case 26 => curNeighbors.map(neigh => (neigh, new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupLeftBegin
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxGhostLeftBegin
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxGhostRightBegin
          }) ++ Array(0)),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => fieldSelection.field.layout(i).idxDupRightEnd
            case i if neigh.dir(i) < 0  => fieldSelection.field.layout(i).idxGhostLeftEnd
            case i if neigh.dir(i) > 0  => fieldSelection.field.layout(i).idxGhostRightEnd
          }) ++ Array(fieldSelection.field.vectorSize)))))
    }
  }

  override def expand : Output[FunctionStatement] = {
    var body = new ListBuffer[Statement]

    val field = fieldSelection.field

    if (begin)
      body += new HandleBoundaries(fieldSelection, genIndicesBoundaryHandling)

    // sync duplicate values
    if (field.communicatesDuplicated) {
      val concurrencyId = (if (begin && finish) 0 else 0)
      if (field.layout.foldLeft(0)((old : Int, l) => old max l.numDupLayersLeft max l.numDupLayersRight) > 0) {
        Knowledge.comm_strategyFragment match {
          case 6 => {
            for (dim <- 0 until Knowledge.dimensionality) {
              var sendNeighbors = ListBuffer(neighbors(2 * dim + 0))
              var recvNeighbors = ListBuffer(neighbors(2 * dim + 1))
              if (Knowledge.domain_canHaveRemoteNeighs) {
                if (begin) {
                  body += new RemoteSends(fieldSelection, genIndicesDuplicateRemoteSend(sendNeighbors), true, false, concurrencyId)
                  if (Knowledge.domain_canHaveLocalNeighs)
                    body += new LocalSend(fieldSelection, genIndicesDuplicateLocalSend(sendNeighbors))
                }
                if (finish) {
                  body += new RemoteRecvs(fieldSelection, genIndicesDuplicateRemoteRecv(recvNeighbors), true, true, concurrencyId)
                  body += new RemoteSends(fieldSelection, genIndicesDuplicateRemoteSend(sendNeighbors), false, true, concurrencyId)
                }
              } else if (Knowledge.domain_canHaveLocalNeighs) {
                if (begin)
                  body += new LocalSend(fieldSelection, genIndicesDuplicateLocalSend(sendNeighbors))
              }
            }
          }
          case 26 => {
            var sendNeighbors = neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0)
            var recvNeighbors = neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0)
            if (Knowledge.domain_canHaveRemoteNeighs) {
              if (begin) {
                body += new RemoteSends(fieldSelection, genIndicesDuplicateRemoteSend(sendNeighbors), true, false, concurrencyId)
                if (Knowledge.domain_canHaveLocalNeighs)
                  body += new LocalSend(fieldSelection, genIndicesDuplicateLocalSend(sendNeighbors))
              }
              if (finish) {
                body += new RemoteRecvs(fieldSelection, genIndicesDuplicateRemoteRecv(recvNeighbors), true, true, concurrencyId)
                body += new RemoteSends(fieldSelection, genIndicesDuplicateRemoteSend(sendNeighbors), false, true, concurrencyId)
              }
            } else if (Knowledge.domain_canHaveLocalNeighs) {
              if (begin)
                body += new LocalSend(fieldSelection, genIndicesDuplicateLocalSend(sendNeighbors))
            }
          }
        }
      }
    }

    // update ghost layers
    if (field.communicatesGhosts) {
      val concurrencyId = (if (begin && finish) 0 else 1)
      if (field.layout.foldLeft(0)((old : Int, l) => old max l.numGhostLayersLeft max l.numGhostLayersRight) > 0) {
        Knowledge.comm_strategyFragment match {
          case 6 => {
            for (dim <- 0 until Knowledge.dimensionality) {
              var curNeighbors = ListBuffer(neighbors(2 * dim + 0), neighbors(2 * dim + 1))
              if (Knowledge.domain_canHaveRemoteNeighs) {
                if (begin) {
                  body += new RemoteSends(fieldSelection, genIndicesGhostRemoteSend(curNeighbors), true, false, concurrencyId)
                  if (Knowledge.domain_canHaveLocalNeighs)
                    body += new LocalSend(fieldSelection, genIndicesGhostLocalSend(curNeighbors))
                }
                if (finish) {
                  body += new RemoteRecvs(fieldSelection, genIndicesGhostRemoteRecv(curNeighbors), true, true, concurrencyId)
                  body += new RemoteSends(fieldSelection, genIndicesGhostRemoteSend(curNeighbors), false, true, concurrencyId)
                }
              } else if (Knowledge.domain_canHaveLocalNeighs) {
                if (begin)
                  body += new LocalSend(fieldSelection, genIndicesGhostLocalSend(curNeighbors))
              }
            }
          }
          case 26 => {
            if (Knowledge.domain_canHaveRemoteNeighs) {
              if (begin) {
                body += new RemoteSends(fieldSelection, genIndicesGhostRemoteSend(neighbors), true, false, concurrencyId)
                if (Knowledge.domain_canHaveLocalNeighs)
                  body += new LocalSend(fieldSelection, genIndicesGhostLocalSend(neighbors))
              }
              if (finish) {
                body += new RemoteRecvs(fieldSelection, genIndicesGhostRemoteRecv(neighbors), true, true, concurrencyId)
                body += new RemoteSends(fieldSelection, genIndicesGhostRemoteSend(neighbors), false, true, concurrencyId)
              }
            } else if (Knowledge.domain_canHaveLocalNeighs) {
              if (begin)
                body += new LocalSend(fieldSelection, genIndicesGhostLocalSend(neighbors))
            }
          }
        }
      }
    }

    // compile function
    var name = (
      if (begin && finish) "exch"
      else if (begin) "beginExch"
      else if (finish) "finishExch"
      else "ERROR")
    name += fieldSelection.codeName

    FunctionStatement(new UnitDatatype(), name,
      ListBuffer(VariableAccess("slot", Some("unsigned int"))),
      body)
  }
}
