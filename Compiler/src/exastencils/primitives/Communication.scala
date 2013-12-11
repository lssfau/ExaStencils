package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class MPI_Receive(var buffer : String, var size : String, var typeName : String, var rank : String, var tag : String, var request : String) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";

    // TODO: move omp stuff to separate class
    s += s"#pragma omp critical\n{\n";

    s += s"MPI_Irecv($buffer, $size, $typeName, $rank, $tag, MPI_COMM_WORLD, &$request);\n"

    s += s"}\n";

    return s;
  }
};

case class RemoteReceive(var field : Field, var level : Any /*FIXME: Int*/ , var neighbors : ListBuffer[NeighInfo]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = RemoteReceive\n";

  def expand : LoopOverFragments = {
    new LoopOverFragments(
      neighbors.map(neigh =>
        (new ifCond(
          s"FRAG_INVALID != fragments[e]->neigh[FRAG_CUBE_${neigh.label} - FRAG_CUBE_ZN_YN_XN].location",
          Array(
            s"FragmentNeighInfo& curNeigh = fragments[e]->neigh[FRAG_CUBE_${neigh.label} - FRAG_CUBE_ZN_YN_XN];",
            (new ifCond(s"curNeigh.isRemote",
              Array(
                new MPI_Receive(
                  s"fragments[e]->recvBuffer_${neigh.label}",
                  s"${Knowledge.numGhostLayers} * fragments[e]->${field.codeName}[slot][$level]->numDataPointsPerDim.y * fragments[e]->${field.codeName}[slot][$level]->numDataPointsPerDim.z",
                  s"MPI_DOUBLE",
                  s"curNeigh.remoteRank",
                  s"((unsigned int)curNeigh.fragId << 16) + ((unsigned int)fragments[e]->id & 0x0000ffff)",
                  s"fragments[e]->request_Recv_${neigh.label}"),
                s"fragments[e]->reqOutstanding_Recv_${neigh.label} = true;"))))))).toArray);
  }
}
