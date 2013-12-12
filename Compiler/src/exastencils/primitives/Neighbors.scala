package exastencils.primitives

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._

case class IsNeighborValid(neigh : NeighInfo) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    return s"curFragment.neighbor_isValid[${neigh.index}]"
  }
}

case class IsNeighborRemote(neigh : NeighInfo) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    return s"curFragment.neighbor_isRemote[${neigh.index}]"
  }
}
