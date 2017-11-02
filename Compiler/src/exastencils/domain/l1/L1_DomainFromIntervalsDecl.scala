package exastencils.domain.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.l1._
import exastencils.parsers.l1.L1_ReservedSigns
import exastencils.prettyprinting.PpStream
import exastencils.util.l1.L1_AABB

/// L1_DomainFromIntervalsDecl

object L1_DomainFromIntervalsDecl {
  def apply(name : String, intervals : List[L1_Interval]) = new L1_DomainFromIntervalsDecl(name, intervals.to[ListBuffer])
}

case class L1_DomainFromIntervalsDecl(var name : String, intervals : ListBuffer[L1_Interval]) extends L1_DomainDecl {
  override def prettyprint(out : PpStream) = {
    out << "Domain " << name << " = " << intervals.map(_.prettyprint()).mkString(s" ${ L1_ReservedSigns.times._1 } ")
  }

  override def addToKnowledge() = {
    val aabb = L1_AABB(intervals.map(_.begin).toArray, intervals.map(_.end).toArray)
    L1_DomainCollection.add(L1_DomainFromAABB(name, aabb))
  }
}
