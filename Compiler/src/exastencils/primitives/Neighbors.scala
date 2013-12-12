package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._

case class getNeighInfo_IsValid(neigh : NeighInfo) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    return s"curFragment.neighbor_isValid[${neigh.index}]"
  }
}

case class getNeighInfo_IsRemote(neigh : NeighInfo) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    return s"curFragment.neighbor_isRemote[${neigh.index}]"
  }
}

case class getNeighInfo_LocalPtr(neigh : NeighInfo) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    return s"curFragment.neighbor_localPtr[${neigh.index}]"
  }
}

case class getNeighInfo_FragmentId(neigh : NeighInfo) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    return s"curFragment.neighbor_fragmentId[${neigh.index}]"
  }
}

case class getNeighInfo_RemoteRank(neigh : NeighInfo) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    return s"curFragment.neighbor_remoteRank[${neigh.index}]"
  }
}
