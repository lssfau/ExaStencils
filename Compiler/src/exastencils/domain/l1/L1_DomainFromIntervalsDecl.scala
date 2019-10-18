//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

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
    out << "Domain " << name << " = " << intervals.map(_.prettyprint()).mkString(s" ${ L1_ReservedSigns.times._2 } ")
  }

  override def addToKnowledge() = {
    val aabb = L1_AABB(intervals.map(_.begin).toArray, intervals.map(_.end).toArray)
    L1_DomainCollection.add(L1_DomainFromAABB(name, aabb))
  }
}
