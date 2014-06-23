package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._

class NeighborInfo(var dir : Array[Int], var index : Int) {
  var label : String = (Knowledge.dimensionality - 1 to 0 by -1).toList.map(i => dimToString(i).toUpperCase + dirToString(dir(i))).mkString("_")
}

case class getNeighInfo_IsValid(var neigh : NeighborInfo, var domain : Int) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = getNeighInfo_IsValid\n"

  override def expand : Expression = {
    iv.NeighborIsValid(domain, neigh.index)
  }
}

case class getNeighInfo_IsInvalid(var neigh : NeighborInfo, var domain : Int) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = getNeighInfo_IsInvalid\n"

  override def expand : Expression = {
    UnaryExpression(UnaryOperators.Not, iv.NeighborIsValid(domain, neigh.index))
  }
}

case class getNeighInfo_IsRemote(var neigh : NeighborInfo, var domain : Int) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = getNeighInfo_IsRemote\n"

  override def expand : Expression = {
    iv.NeighborIsRemote(domain, neigh.index)
  }
}

case class getNeighInfo_IsValidAndRemote(var neigh : NeighborInfo, var domain : Int) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = getNeighInfo_IsValidAndRemote\n"

  override def expand : Expression = {
    iv.NeighborIsValid(domain, neigh.index) AndAnd iv.NeighborIsRemote(domain, neigh.index)
  }
}

case class getNeighInfo_IsValidAndNotRemote(var neigh : NeighborInfo, var domain : Int) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = getNeighInfo_IsValidAndNotRemote\n"

  override def expand : Expression = {
    iv.NeighborIsValid(domain, neigh.index) AndAnd UnaryExpression(UnaryOperators.Not, iv.NeighborIsRemote(domain, neigh.index))
  }
}

case class getNeighInfo_LocalId(var neigh : NeighborInfo, var domain : Int) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = getNeighInfo_FragmentId\n"

  override def expand : Expression = {
    iv.NeighborFragLocalId(domain, neigh.index)
  }
}

case class getNeighInfo_RemoteRank(var neigh : NeighborInfo, var domain : Int) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = getNeighInfo_RemoteRank\n"

  override def expand : Expression = {
    iv.NeighborRemoteRank(domain, neigh.index)
  }
}
