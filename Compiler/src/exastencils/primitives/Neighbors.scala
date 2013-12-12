package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._

case class getNeighInfo_IsValid(var neigh : NeighInfo) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    return s"curFragment.neighbor_isValid[${neigh.index}]"
  }
}

case class getNeighInfo_IsRemote(var neigh : NeighInfo) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    return s"curFragment.neighbor_isRemote[${neigh.index}]"
  }
}

case class getNeighInfo_LocalPtr(var neigh : NeighInfo) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    return s"curFragment.neighbor_localPtr[${neigh.index}]"
  }
}

case class getNeighInfo_FragmentId(var neigh : NeighInfo) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    return s"curFragment.neighbor_fragmentId[${neigh.index}]"
  }
}

case class getNeighInfo_RemoteRank(var neigh : NeighInfo) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    return s"curFragment.neighbor_remoteRank[${neigh.index}]"
  }
}
