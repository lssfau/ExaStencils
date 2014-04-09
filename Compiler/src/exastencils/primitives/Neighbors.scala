package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._

case class getNeighInfo_IsValid(var neigh : NeighborInfo, var domain : Int) extends Expression {
  def cpp : String = { s"curFragment.neighbor_isValid[$domain][${neigh.index}]"; }
}

case class getNeighInfo_IsInvalid(var neigh : NeighborInfo, var domain : Int) extends Expression {
  def cpp : String = { s"!curFragment.neighbor_isValid[$domain][${neigh.index}]"; }
}

case class getNeighInfo_IsRemote(var neigh : NeighborInfo, var domain : Int) extends Expression {
  def cpp : String = { s"curFragment.neighbor_isRemote[$domain][${neigh.index}]"; }
}

case class getNeighInfo_IsValidAndRemote(var neigh : NeighborInfo, var domain : Int) extends Expression {
  def cpp : String = {
    s"curFragment.neighbor_isRemote[$domain][${neigh.index}]"; // remote neighbors should always be valid
    //s"curFragment.neighbor_isValid[${neigh.index}] && curFragment.neighbor_isRemote[${neigh.index}]"
  }
}

case class getNeighInfo_IsValidAndNotRemote(var neigh : NeighborInfo, var domain : Int) extends Expression {
  def cpp : String = { s"curFragment.neighbor_isValid[$domain][${neigh.index}] && !curFragment.neighbor_isRemote[$domain][${neigh.index}]" }
}

case class getNeighInfo_LocalPtr(var neigh : NeighborInfo, var domain : Int) extends Expression {
  def cpp : String = { s"curFragment.neighbor_localPtr[$domain][${neigh.index}]"; }
}

case class getNeighInfo_FragmentId(var neigh : NeighborInfo, var domain : Int) extends Expression {
  def cpp : String = { s"curFragment.neighbor_fragmentId[$domain][${neigh.index}]"; }
}

case class getNeighInfo_RemoteRank(var neigh : NeighborInfo, var domain : Int) extends Expression {
  def cpp : String = { s"curFragment.neighbor_remoteRank[$domain][${neigh.index}]"; }
}
