package exastencils.communication

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.multiGrid._
import exastencils.prettyprinting._

// FIXME: Think about moving all of this index information to some other source. Maybe some kind of ... DSL ... or even Layer4

abstract class FieldBoundaryFunction() extends AbstractFunctionStatement with Expandable {
  var fieldSelection : FieldSelection
  def insideFragLoop : Boolean

  def compileName : String
  def compileBody(updatedFieldSelection : FieldSelection) : ListBuffer[Statement]

  def resolveIndex(indexId : String, dim : Int) : Expression = {
    if (Knowledge.experimental_useLevelIndepFcts) {
      // FIXME
      ??? //ArrayAccess(iv.IndexFromField(fieldSelection.field.identifier, "level", indexId), dim)
    } else {
      fieldSelection.field.fieldLayout.idxById(indexId, dim)
    }
  }

  def vecFieldIndexBegin = Array(fieldSelection.arrayIndex.getOrElse(0).toLong : Expression)

  def vecFieldIndexEnd = {
    if (fieldSelection.arrayIndex.isDefined)
      Array((fieldSelection.arrayIndex.get + 1) : Expression)
    else
      Array(fieldSelection.field.vectorSize : Expression)
  }

  override def expand : Output[FunctionStatement] = {
    var body = new ListBuffer[Statement]

    val updatedFieldSelection = if (Knowledge.experimental_useLevelIndepFcts) {
      val updatedFieldSelection = Duplicate(fieldSelection)
      for (dim <- 0 until Knowledge.dimensionality)
        updatedFieldSelection.field.fieldLayout(dim).total = ArrayAccess(iv.IndexFromField(fieldSelection.field.identifier, "level", "TOT"), dim)
      updatedFieldSelection.level = "level"
      updatedFieldSelection
    } else {
      fieldSelection
    }

    var fctArgs : ListBuffer[VariableAccess] = ListBuffer()
    fctArgs += VariableAccess("slot", Some("unsigned int"))
    if (Knowledge.experimental_useLevelIndepFcts)
      VariableAccess("level", Some("unsigned int"))
    if (insideFragLoop)
      fctArgs += VariableAccess(LoopOverFragments.defIt, Some("int"))

    FunctionStatement(UnitDatatype, compileName, fctArgs, compileBody(updatedFieldSelection))
  }
}

case class ApplyBCsFunction(var name : String, override var fieldSelection : FieldSelection, var neighbors : ListBuffer[NeighborInfo],
    var insideFragLoop : Boolean) extends FieldBoundaryFunction {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ApplyBCsFunction\n"
  override def prettyprint_decl = prettyprint

  def genIndicesBoundaryHandling(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange)] = {
    // FIXME: this works for now, but might be adapted later to incorporate different regions of boundary handling
    curNeighbors.map(neigh => (neigh, new IndexRange(
      new MultiIndex(
        DimArray().map(i =>
          fieldSelection.fieldLayout.discretization match {
            case d if "node" == d
              || ("face_x" == d && 0 == i)
              || ("face_y" == d && 1 == i)
              || ("face_z" == d && 2 == i) => i match {
              case i if neigh.dir(i) == 0 => resolveIndex("DLB", i) // DLB, GLB
              case i if neigh.dir(i) < 0  => resolveIndex("DLB", i) // DLB, GLB
              case i if neigh.dir(i) > 0  => resolveIndex("DRB", i)
            }
            case d if "cell" == d
              || ("face_x" == d && 0 != i)
              || ("face_y" == d && 1 != i)
              || ("face_z" == d && 2 != i) => i match {
              case i if neigh.dir(i) == 0 => resolveIndex("DLB", i) // DLB, GLB
              case i if neigh.dir(i) < 0  => resolveIndex("DLB", i)
              case i if neigh.dir(i) > 0  => resolveIndex("DRB", i) - 1
            }
          }) ++ vecFieldIndexBegin),
      new MultiIndex(
        DimArray().map(i =>
          fieldSelection.fieldLayout.discretization match {
            case d if "node" == d
              || ("face_x" == d && 0 == i)
              || ("face_y" == d && 1 == i)
              || ("face_z" == d && 2 == i) => i match {
              case i if neigh.dir(i) == 0 => resolveIndex("DRE", i) // DRE, GRE
              case i if neigh.dir(i) < 0  => resolveIndex("DLE", i)
              case i if neigh.dir(i) > 0  => resolveIndex("DRE", i) // DRE, GRE
            }
            case d if "cell" == d
              || ("face_x" == d && 0 != i)
              || ("face_y" == d && 1 != i)
              || ("face_z" == d && 2 != i) => i match {
              case i if neigh.dir(i) == 0 => resolveIndex("DRE", i) // DRE, GRE
              case i if neigh.dir(i) < 0  => resolveIndex("DLE", i) + 1
              case i if neigh.dir(i) > 0  => resolveIndex("DRE", i)
            }
          }) ++ vecFieldIndexEnd))))
  }

  override def compileName : String = name
  override def compileBody(updatedFieldSelection : FieldSelection) : ListBuffer[Statement] = {
    var body = new ListBuffer[Statement]

    val boundaryNeighs = neighbors.filter(neigh => {
      var numNonZeros = 0
      for (dim <- 0 until Knowledge.dimensionality)
        if (0 != neigh.dir(dim))
          numNonZeros += 1
      (1 == numNonZeros)
    })
    body += new HandleBoundaries(updatedFieldSelection, genIndicesBoundaryHandling(boundaryNeighs))

    body
  }
}

case class ExchangeDataFunction(var name : String, override var fieldSelection : FieldSelection, var neighbors : ListBuffer[NeighborInfo],
    var begin : Boolean, var finish : Boolean,
    var dupLayerExch : Boolean, var dupLayerBegin : MultiIndex, var dupLayerEnd : MultiIndex,
    var ghostLayerExch : Boolean, var ghostLayerBegin : MultiIndex, var ghostLayerEnd : MultiIndex,
    var insideFragLoop : Boolean) extends FieldBoundaryFunction {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ExchangeDataFunction\n"
  override def prettyprint_decl = prettyprint

  def genIndicesDuplicateRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange)] = {
    curNeighbors.map(neigh => (neigh, new IndexRange(
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("DLB", i)
            case 26 => resolveIndex("IB", i)
          }
          case i if neigh.dir(i) < 0 => resolveIndex("DLB", i) + dupLayerBegin(i)
          case i if neigh.dir(i) > 0 => resolveIndex("DRE", i) - dupLayerEnd(i)
        }) ++ vecFieldIndexBegin),
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("DRE", i)
            case 26 => resolveIndex("DRE", i)
          }
          case i if neigh.dir(i) < 0 => resolveIndex("DLB", i) + dupLayerEnd(i)
          case i if neigh.dir(i) > 0 => resolveIndex("DRE", i) - dupLayerBegin(i)
        }) ++ vecFieldIndexEnd))))
  }

  def genIndicesDuplicateLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange, IndexRange)] = {
    curNeighbors.map(neigh => (neigh,
      new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DLB", i)
              case 26 => resolveIndex("IB", i)
            }
            case i if neigh.dir(i) < 0 => resolveIndex("DLB", i) + dupLayerBegin(i)
            case i if neigh.dir(i) > 0 => resolveIndex("DRB", i) - dupLayerEnd(i)
          }) ++ vecFieldIndexBegin),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DRE", i)
              case 26 => resolveIndex("DRE", i)
            }
            case i if neigh.dir(i) < 0 => resolveIndex("DLE", i) + dupLayerEnd(i)
            case i if neigh.dir(i) > 0 => resolveIndex("DRE", i) - dupLayerBegin(i)
          }) ++ vecFieldIndexEnd)),
      new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DLB", i)
              case 26 => resolveIndex("IB", i)
            }
            case i if -neigh.dir(i) < 0 => resolveIndex("DLB", i) - dupLayerEnd(i)
            case i if -neigh.dir(i) > 0 => resolveIndex("DRB", i) + dupLayerBegin(i)
          }) ++ vecFieldIndexBegin),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DRE", i)
              case 26 => resolveIndex("DRE", i)
            }
            case i if -neigh.dir(i) < 0 => resolveIndex("DLE", i) - dupLayerBegin(i)
            case i if -neigh.dir(i) > 0 => resolveIndex("DRE", i) + dupLayerEnd(i)
          }) ++ vecFieldIndexEnd))))
  }

  def genIndicesDuplicateLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange, IndexRange)] = {
    curNeighbors.map(neigh => (neigh,
      new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DLB", i)
              case 26 => resolveIndex("IB", i)
            }
            case i if neigh.dir(i) < 0 => resolveIndex("DLB", i) - dupLayerEnd(i)
            case i if neigh.dir(i) > 0 => resolveIndex("DRB", i) + dupLayerBegin(i)
          }) ++ vecFieldIndexBegin),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DRE", i)
              case 26 => resolveIndex("DRE", i)
            }
            case i if neigh.dir(i) < 0 => resolveIndex("DLE", i) - dupLayerBegin(i)
            case i if neigh.dir(i) > 0 => resolveIndex("DRE", i) + dupLayerEnd(i)
          }) ++ vecFieldIndexEnd)),
      new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DLB", i)
              case 26 => resolveIndex("IB", i)
            }
            case i if -neigh.dir(i) < 0 => resolveIndex("DLB", i) + dupLayerBegin(i)
            case i if -neigh.dir(i) > 0 => resolveIndex("DRB", i) - dupLayerEnd(i)
          }) ++ vecFieldIndexBegin),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("DRE", i)
              case 26 => resolveIndex("DRE", i)
            }
            case i if -neigh.dir(i) < 0 => resolveIndex("DLE", i) + dupLayerEnd(i)
            case i if -neigh.dir(i) > 0 => resolveIndex("DRE", i) - dupLayerBegin(i)
          }) ++ vecFieldIndexEnd))))
  }

  def genIndicesDuplicateRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange)] = {
    curNeighbors.map(neigh => (neigh, new IndexRange(
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("DLB", i)
            case 26 => resolveIndex("IB", i)
          }
          case i if neigh.dir(i) < 0 => resolveIndex("DLE", i) - dupLayerEnd(i)
          case i if neigh.dir(i) > 0 => resolveIndex("DRB", i) + dupLayerBegin(i)
        }) ++ vecFieldIndexBegin),
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("DRE", i)
            case 26 => resolveIndex("DRE", i)
          }
          case i if neigh.dir(i) < 0 => resolveIndex("DLE", i) - dupLayerBegin(i)
          case i if neigh.dir(i) > 0 => resolveIndex("DRB", i) + dupLayerEnd(i)
        }) ++ vecFieldIndexEnd))))
  }

  def genIndicesGhostRemoteSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange)] = {
    curNeighbors.map(neigh => (neigh, new IndexRange(
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("GLB", i)
            case 26 => resolveIndex("DLB", i)
          }
          case i if neigh.dir(i) < 0 => resolveIndex("IB", i) + ghostLayerBegin(i)
          case i if neigh.dir(i) > 0 => resolveIndex("IE", i) - ghostLayerEnd(i)
        }) ++ vecFieldIndexBegin),
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("GRE", i)
            case 26 => resolveIndex("DRE", i)
          }
          case i if neigh.dir(i) < 0 => resolveIndex("IB", i) + ghostLayerEnd(i)
          case i if neigh.dir(i) > 0 => resolveIndex("IE", i) - ghostLayerBegin(i)
        }) ++ vecFieldIndexEnd))))
  }

  def genIndicesGhostLocalSend(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange, IndexRange)] = {
    curNeighbors.map(neigh => (neigh,
      new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GLB", i)
              case 26 => resolveIndex("DLB", i)
            }
            case i if neigh.dir(i) < 0 => resolveIndex("IB", i) + ghostLayerBegin(i)
            case i if neigh.dir(i) > 0 => resolveIndex("IE", i) - ghostLayerEnd(i)
          }) ++ vecFieldIndexBegin),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GRE", i)
              case 26 => resolveIndex("DRE", i)
            }
            case i if neigh.dir(i) < 0 => resolveIndex("IB", i) + ghostLayerEnd(i)
            case i if neigh.dir(i) > 0 => resolveIndex("IE", i) - ghostLayerBegin(i)
          }) ++ vecFieldIndexEnd)),
      new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GLB", i)
              case 26 => resolveIndex("DLB", i)
            }
            case i if -neigh.dir(i) < 0 => resolveIndex("GLE", i) - ghostLayerEnd(i)
            case i if -neigh.dir(i) > 0 => resolveIndex("GRB", i) + ghostLayerBegin(i)
          }) ++ vecFieldIndexBegin),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GRE", i)
              case 26 => resolveIndex("DRE", i)
            }
            case i if -neigh.dir(i) < 0 => resolveIndex("GLE", i) - ghostLayerBegin(i)
            case i if -neigh.dir(i) > 0 => resolveIndex("GRB", i) + ghostLayerEnd(i)
          }) ++ vecFieldIndexEnd))))
  }

  def genIndicesGhostLocalRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange, IndexRange)] = {
    curNeighbors.map(neigh => (neigh,
      new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GLB", i)
              case 26 => resolveIndex("DLB", i)
            }
            case i if neigh.dir(i) < 0 => resolveIndex("GLE", i) - ghostLayerEnd(i)
            case i if neigh.dir(i) > 0 => resolveIndex("GRB", i) + ghostLayerBegin(i)
          }) ++ vecFieldIndexBegin),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GRE", i)
              case 26 => resolveIndex("DRE", i)
            }
            case i if neigh.dir(i) < 0 => resolveIndex("GLE", i) - ghostLayerBegin(i)
            case i if neigh.dir(i) > 0 => resolveIndex("GRB", i) + ghostLayerEnd(i)
          }) ++ vecFieldIndexEnd)),
      new IndexRange(
        new MultiIndex(
          DimArray().map(i => i match {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GLB", i)
              case 26 => resolveIndex("DLB", i)
            }
            case i if -neigh.dir(i) < 0 => resolveIndex("IB", i) + ghostLayerBegin(i)
            case i if -neigh.dir(i) > 0 => resolveIndex("IE", i) - ghostLayerEnd(i)
          }) ++ vecFieldIndexBegin),
        new MultiIndex(
          DimArray().map(i => i match {
            case i if -neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
              case 6  => resolveIndex("GRE", i)
              case 26 => resolveIndex("DRE", i)
            }
            case i if -neigh.dir(i) < 0 => resolveIndex("IB", i) + ghostLayerEnd(i)
            case i if -neigh.dir(i) > 0 => resolveIndex("IE", i) - ghostLayerBegin(i)
          }) ++ vecFieldIndexEnd))))
  }

  def genIndicesGhostRemoteRecv(curNeighbors : ListBuffer[NeighborInfo]) : ListBuffer[(NeighborInfo, IndexRange)] = {
    curNeighbors.map(neigh => (neigh, new IndexRange(
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("GLB", i)
            case 26 => resolveIndex("DLB", i)
          }
          case i if neigh.dir(i) < 0 => resolveIndex("GLE", i) - ghostLayerEnd(i)
          case i if neigh.dir(i) > 0 => resolveIndex("GRB", i) + ghostLayerBegin(i)
        }) ++ vecFieldIndexBegin),
      new MultiIndex(
        DimArray().map(i => i match {
          case i if neigh.dir(i) == 0 => Knowledge.comm_strategyFragment match {
            case 6  => resolveIndex("GRE", i)
            case 26 => resolveIndex("DRE", i)
          }
          case i if neigh.dir(i) < 0 => resolveIndex("GLE", i) - ghostLayerBegin(i)
          case i if neigh.dir(i) > 0 => resolveIndex("GRB", i) + ghostLayerEnd(i)
        }) ++ vecFieldIndexEnd))))
  }

  override def compileName : String = name
  override def compileBody(updatedFieldSelection : FieldSelection) : ListBuffer[Statement] = {
    var body = new ListBuffer[Statement]
    val field = updatedFieldSelection.field

    // sync duplicate values
    if (dupLayerExch && field.communicatesDuplicated) {
      val concurrencyId = (if (begin && finish) 0 else 0)
      if (field.fieldLayout.layoutsPerDim.foldLeft(0)((old : Int, l) => old max l.numDupLayersLeft max l.numDupLayersRight) > 0) {
        Knowledge.comm_strategyFragment match {
          case 6 => {
            for (dim <- 0 until Knowledge.dimensionality) {
              var recvNeighbors = ListBuffer(neighbors(2 * dim + 0))
              var sendNeighbors = ListBuffer(neighbors(2 * dim + 1))

              if (begin) {
                body += new RemoteSends(updatedFieldSelection, genIndicesDuplicateRemoteSend(sendNeighbors), true, false, concurrencyId, insideFragLoop)
                body += new RemoteRecvs(updatedFieldSelection, genIndicesDuplicateRemoteRecv(recvNeighbors), true, false, concurrencyId, insideFragLoop)
                body += new StartLocalComm(updatedFieldSelection, genIndicesDuplicateLocalSend(sendNeighbors), genIndicesDuplicateLocalRecv(recvNeighbors), insideFragLoop)
              }
              if (finish) {
                body += new RemoteRecvs(updatedFieldSelection, genIndicesDuplicateRemoteRecv(recvNeighbors), false, true, concurrencyId, insideFragLoop)
                body += new RemoteSends(updatedFieldSelection, genIndicesDuplicateRemoteSend(sendNeighbors), false, true, concurrencyId, insideFragLoop)
                body += new FinishLocalComm(updatedFieldSelection, genIndicesDuplicateLocalSend(sendNeighbors), genIndicesDuplicateLocalRecv(recvNeighbors), insideFragLoop)
              }
            }
          }
          case 26 => {
            var sendNeighbors = neighbors.filter(neigh => neigh.dir(0) >= 0 && neigh.dir(1) >= 0 && neigh.dir(2) >= 0)
            var recvNeighbors = neighbors.filter(neigh => neigh.dir(0) <= 0 && neigh.dir(1) <= 0 && neigh.dir(2) <= 0)
            if (begin) {
              body += new RemoteSends(updatedFieldSelection, genIndicesDuplicateRemoteSend(sendNeighbors), true, false, concurrencyId, insideFragLoop)
              body += new RemoteRecvs(updatedFieldSelection, genIndicesDuplicateRemoteRecv(recvNeighbors), true, false, concurrencyId, insideFragLoop)
              body += new StartLocalComm(updatedFieldSelection, genIndicesDuplicateLocalSend(sendNeighbors), genIndicesDuplicateLocalRecv(recvNeighbors), insideFragLoop)
            }
            if (finish) {
              body += new RemoteRecvs(updatedFieldSelection, genIndicesDuplicateRemoteRecv(recvNeighbors), false, true, concurrencyId, insideFragLoop)
              body += new RemoteSends(updatedFieldSelection, genIndicesDuplicateRemoteSend(sendNeighbors), false, true, concurrencyId, insideFragLoop)
              body += new FinishLocalComm(updatedFieldSelection, genIndicesDuplicateLocalSend(sendNeighbors), genIndicesDuplicateLocalRecv(recvNeighbors), insideFragLoop)
            }
          }
        }
      }
    }

    // update ghost layers
    if (ghostLayerExch && field.communicatesGhosts) {
      val concurrencyId = (if (begin && finish) 0 else 1)
      if (field.fieldLayout.layoutsPerDim.foldLeft(0)((old : Int, l) => old max l.numGhostLayersLeft max l.numGhostLayersRight) > 0) {
        Knowledge.comm_strategyFragment match {
          case 6 => {
            for (dim <- 0 until Knowledge.dimensionality) {
              var curNeighbors = ListBuffer(neighbors(2 * dim + 0), neighbors(2 * dim + 1))

              if (begin) {
                body += new RemoteSends(updatedFieldSelection, genIndicesGhostRemoteSend(curNeighbors), true, false, concurrencyId, insideFragLoop)
                body += new RemoteRecvs(updatedFieldSelection, genIndicesGhostRemoteRecv(curNeighbors), true, false, concurrencyId, insideFragLoop)
                body += new StartLocalComm(updatedFieldSelection, genIndicesGhostLocalSend(curNeighbors), genIndicesGhostLocalRecv(curNeighbors), insideFragLoop)
              }
              if (finish) {
                body += new RemoteRecvs(updatedFieldSelection, genIndicesGhostRemoteRecv(curNeighbors), false, true, concurrencyId, insideFragLoop)
                body += new RemoteSends(updatedFieldSelection, genIndicesGhostRemoteSend(curNeighbors), false, true, concurrencyId, insideFragLoop)
                body += new FinishLocalComm(updatedFieldSelection, genIndicesGhostLocalSend(curNeighbors), genIndicesGhostLocalRecv(curNeighbors), insideFragLoop)
              }
            }
          }
          case 26 => {
            if (begin) {
              body += new RemoteSends(updatedFieldSelection, genIndicesGhostRemoteSend(neighbors), true, false, concurrencyId, insideFragLoop)
              body += new RemoteRecvs(updatedFieldSelection, genIndicesGhostRemoteRecv(neighbors), true, false, concurrencyId, insideFragLoop)
              body += new StartLocalComm(updatedFieldSelection, genIndicesGhostLocalSend(neighbors), genIndicesGhostLocalRecv(neighbors), insideFragLoop)
            }
            if (finish) {
              body += new RemoteRecvs(updatedFieldSelection, genIndicesGhostRemoteRecv(neighbors), false, true, concurrencyId, insideFragLoop)
              body += new RemoteSends(updatedFieldSelection, genIndicesGhostRemoteSend(neighbors), false, true, concurrencyId, insideFragLoop)
              body += new FinishLocalComm(updatedFieldSelection, genIndicesGhostLocalSend(neighbors), genIndicesGhostLocalRecv(neighbors), insideFragLoop)
            }
          }
        }
      }
    }

    body
  }
}
