package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._

// FIXME: Think about moving all of this information to some other source. Maybe some kind of ... DSL ... or even Level4

case class ExchangeData_6(var fieldSelection : FieldSelection, var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ExchangeData_6\n"
  override def cpp_decl = cpp

  override def expand : Output[FunctionStatement] = {
    var body = new ListBuffer[Statement]

    val field = fieldSelection.field

    // handle BC
    // FIXME: currently treats numInnerLayers points
    // FIXME: this treats more points than required (outer halo)
    body += new HandleBoundaries(fieldSelection, neighbors.map(neigh => (neigh, new IndexRange(
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin //idxDupLeftBegin, idxGhostLeftBegin
          case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin //idxDupLeftBegin, idxGhostLeftBegin
          case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
        }) ++ Array(0)),
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd //idxDupRightEnd, idxGhostRightEnd
          case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
          case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd //idxDupRightEnd, idxGhostRightEnd
        }) ++ Array(field.vectorSize))))))

    // sync duplicate values
    if (field.communicatesDuplicated) {
      if (field.layout.foldLeft(0)((old : Int, l) => old max l.numDupLayersLeft max l.numDupLayersRight) > 0) {
        for (dim <- 0 until Knowledge.dimensionality) {
          val sendRemoteData = ListBuffer(neighbors(2 * dim + 1)).map(neigh => (neigh, new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
              }) ++ Array(0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
                case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
              }) ++ Array(field.vectorSize)))))
          val sendLocalData = ListBuffer(neighbors(2 * dim + 1)).map(neigh => (neigh, new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
              }) ++ Array(0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
                case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
              }) ++ Array(field.vectorSize))),
            new IndexRange(
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if -neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                  case i if -neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
                  case i if -neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
                }) ++ Array(0)),
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if -neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                  case i if -neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
                  case i if -neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
                }) ++ Array(field.vectorSize)))))
          val recvRemoteData = ListBuffer(neighbors(2 * dim + 0)).map(neigh => (neigh, new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
              }) ++ Array(0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
                case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
              }) ++ Array(field.vectorSize)))))

          if (Knowledge.domain_canHaveRemoteNeighs) {
            body += new RemoteSends(fieldSelection, sendRemoteData, true, false)
            if (Knowledge.domain_canHaveLocalNeighs)
              body += new LocalSend(fieldSelection, sendLocalData)
            body += new RemoteRecvs(fieldSelection, recvRemoteData, true, true)
            body += new RemoteSends(fieldSelection, sendRemoteData, false, true)
          } else if (Knowledge.domain_canHaveLocalNeighs) {
            body += new LocalSend(fieldSelection, sendLocalData)
          }
        }
      }
    }

    // update ghost layers
    if (field.communicatesGhosts) {
      if (field.layout.foldLeft(0)((old : Int, l) => old max l.numGhostLayersLeft max l.numGhostLayersRight) > 0) {
        for (dim <- 0 until Knowledge.dimensionality) {
          var curNeighbors = ListBuffer(neighbors(2 * dim + 0), neighbors(2 * dim + 1))
          val sendRemoteData = curNeighbors.map(neigh => (neigh, new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxGhostLeftBegin
                case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin
                case i if neigh.dir(i) > 0  => field.layout(i).idxInnerBegin + field.layout(i).numInnerLayers - field.layout(i).numGhostLayersRight
              }) ++ Array(0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxGhostRightEnd
                case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin + field.layout(i).numGhostLayersLeft
                case i if neigh.dir(i) > 0  => field.layout(i).idxInnerEnd
              }) ++ Array(field.vectorSize)))))
          val sendLocalData = curNeighbors.map(neigh => (neigh,
            new IndexRange(
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if neigh.dir(i) == 0 => field.layout(i).idxGhostLeftBegin
                  case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin
                  case i if neigh.dir(i) > 0  => field.layout(i).idxInnerBegin + field.layout(i).numInnerLayers - field.layout(i).numGhostLayersRight
                }) ++ Array(0)),
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if neigh.dir(i) == 0 => field.layout(i).idxGhostRightEnd
                  case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin + field.layout(i).numGhostLayersLeft
                  case i if neigh.dir(i) > 0  => field.layout(i).idxInnerEnd
                }) ++ Array(field.vectorSize))),
            new IndexRange(
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if -neigh.dir(i) == 0 => field.layout(i).idxGhostLeftBegin
                  case i if -neigh.dir(i) < 0  => field.layout(i).idxGhostLeftBegin
                  case i if -neigh.dir(i) > 0  => field.layout(i).idxGhostRightBegin
                }) ++ Array(0)),
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if -neigh.dir(i) == 0 => field.layout(i).idxGhostRightEnd
                  case i if -neigh.dir(i) < 0  => field.layout(i).idxGhostLeftEnd
                  case i if -neigh.dir(i) > 0  => field.layout(i).idxGhostRightEnd
                }) ++ Array(field.vectorSize)))))
          val recvRemoteData = curNeighbors.map(neigh => (neigh,
            new IndexRange(
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if neigh.dir(i) == 0 => field.layout(i).idxGhostLeftBegin
                  case i if neigh.dir(i) < 0  => field.layout(i).idxGhostLeftBegin
                  case i if neigh.dir(i) > 0  => field.layout(i).idxGhostRightBegin
                }) ++ Array(0)),
              new MultiIndex(
                DimArray().map(i => i match {
                  case i if neigh.dir(i) == 0 => field.layout(i).idxGhostRightEnd
                  case i if neigh.dir(i) < 0  => field.layout(i).idxGhostLeftEnd
                  case i if neigh.dir(i) > 0  => field.layout(i).idxGhostRightEnd
                }) ++ Array(field.vectorSize)))))

          if (Knowledge.domain_canHaveRemoteNeighs) {
            body += new RemoteSends(fieldSelection, sendRemoteData, true, false)
            if (Knowledge.domain_canHaveLocalNeighs)
              body += new LocalSend(fieldSelection, sendLocalData)
            body += new RemoteRecvs(fieldSelection, recvRemoteData, true, true)
            body += new RemoteSends(fieldSelection, sendRemoteData, false, true)
          } else if (Knowledge.domain_canHaveLocalNeighs) {
            body += new LocalSend(fieldSelection, sendLocalData)
          }
        }
      }
    }

    // compile return value
    return FunctionStatement(new UnitDatatype(), s"exch${fieldSelection.codeName}",
      ListBuffer(VariableAccess("slot", Some("unsigned int"))),
      body)
  }
}

case class ExchangeData_26(var fieldSelection : FieldSelection, var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ExchangeData_26\n"
  override def cpp_decl = cpp

  override def expand : Output[FunctionStatement] = {
    var body = new ListBuffer[Statement]

    val field = fieldSelection.field

    // handle BC
    // FIXME: currently treats numInnerLayers points
    body += new HandleBoundaries(fieldSelection, neighbors.map(neigh => (neigh, new IndexRange(
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
          case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
          case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
        }) ++ Array(0)),
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
          case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
          case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
        }) ++ Array(field.vectorSize))))))

    // sync duplicate values
    if (field.communicatesDuplicated) {
      if (field.layout.foldLeft(0)((old : Int, l) => old max l.numDupLayersLeft max l.numDupLayersRight) > 0) {
        val sendRemoteData = neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0).map(neigh => (neigh, new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxInnerBegin
              case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
              case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
            }) ++ Array(0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxInnerEnd
              case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
              case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
            }) ++ Array(field.vectorSize)))))
        val sendLocalData = neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0).map(neigh => (neigh, new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxInnerBegin
              case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
              case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
            }) ++ Array(0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxInnerEnd
              case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
              case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
            }) ++ Array(field.vectorSize))),
          new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if -neigh.dir(i) == 0 => field.layout(i).idxInnerBegin
                case i if -neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
                case i if -neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
              }) ++ Array(0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if -neigh.dir(i) == 0 => field.layout(i).idxInnerEnd
                case i if -neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
                case i if -neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
              }) ++ Array(field.vectorSize)))))
        val recvRemoteData = neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0).map(neigh => (neigh, new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxInnerBegin
              case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftBegin
              case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightBegin
            }) ++ Array(0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxInnerEnd
              case i if neigh.dir(i) < 0  => field.layout(i).idxDupLeftEnd
              case i if neigh.dir(i) > 0  => field.layout(i).idxDupRightEnd
            }) ++ Array(field.vectorSize)))))

        if (Knowledge.domain_canHaveRemoteNeighs) {
          body += new RemoteSends(fieldSelection, sendRemoteData, true, false)
          if (Knowledge.domain_canHaveLocalNeighs)
            body += new LocalSend(fieldSelection, sendLocalData)
          body += new RemoteRecvs(fieldSelection, recvRemoteData, true, true)
          body += new RemoteSends(fieldSelection, sendRemoteData, false, true)
        } else if (Knowledge.domain_canHaveLocalNeighs) {
          body += new LocalSend(fieldSelection, sendLocalData)
        }
      }
    }

    // update ghost layers
    if (field.communicatesGhosts) {
      if (field.layout.foldLeft(0)((old : Int, l) => old max l.numGhostLayersLeft max l.numGhostLayersRight) > 0) {
        val sendRemoteData = neighbors.map(neigh => (neigh, new IndexRange(
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
              case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin
              case i if neigh.dir(i) > 0  => field.layout(i).idxInnerBegin + field.layout(i).numInnerLayers - field.layout(i).numGhostLayersRight
            }) ++ Array(0)),
          new MultiIndex(
            DimArray().map(i => i match {
              case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
              case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin + field.layout(i).numGhostLayersLeft
              case i if neigh.dir(i) > 0  => field.layout(i).idxInnerEnd
            }) ++ Array(field.vectorSize)))))
        val sendLocalData = neighbors.map(neigh => (neigh,
          new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin
                case i if neigh.dir(i) > 0  => field.layout(i).idxInnerBegin + field.layout(i).numInnerLayers - field.layout(i).numGhostLayersRight
              }) ++ Array(0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                case i if neigh.dir(i) < 0  => field.layout(i).idxInnerBegin + field.layout(i).numGhostLayersLeft
                case i if neigh.dir(i) > 0  => field.layout(i).idxInnerEnd
              }) ++ Array(field.vectorSize))),
          new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if -neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                case i if -neigh.dir(i) < 0  => field.layout(i).idxGhostLeftBegin
                case i if -neigh.dir(i) > 0  => field.layout(i).idxGhostRightBegin
              }) ++ Array(0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if -neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                case i if -neigh.dir(i) < 0  => field.layout(i).idxGhostLeftEnd
                case i if -neigh.dir(i) > 0  => field.layout(i).idxGhostRightEnd
              }) ++ Array(field.vectorSize)))))
        val recvRemoteData = neighbors.map(neigh => (neigh,
          new IndexRange(
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupLeftBegin
                case i if neigh.dir(i) < 0  => field.layout(i).idxGhostLeftBegin
                case i if neigh.dir(i) > 0  => field.layout(i).idxGhostRightBegin
              }) ++ Array(0)),
            new MultiIndex(
              DimArray().map(i => i match {
                case i if neigh.dir(i) == 0 => field.layout(i).idxDupRightEnd
                case i if neigh.dir(i) < 0  => field.layout(i).idxGhostLeftEnd
                case i if neigh.dir(i) > 0  => field.layout(i).idxGhostRightEnd
              }) ++ Array(field.vectorSize)))))

        if (Knowledge.domain_canHaveRemoteNeighs) {
          body += new RemoteSends(fieldSelection, sendRemoteData, true, false)
          if (Knowledge.domain_canHaveLocalNeighs)
            body += new LocalSend(fieldSelection, sendLocalData)
          body += new RemoteRecvs(fieldSelection, recvRemoteData, true, true)
          body += new RemoteSends(fieldSelection, sendRemoteData, false, true)
        } else if (Knowledge.domain_canHaveLocalNeighs) {
          body += new LocalSend(fieldSelection, sendLocalData)
        }
      }
    }

    // compile return value
    return FunctionStatement(new UnitDatatype(), s"exch${fieldSelection.codeName}",
      ListBuffer(VariableAccess("slot", Some("unsigned int"))),
      body)
  }
}
