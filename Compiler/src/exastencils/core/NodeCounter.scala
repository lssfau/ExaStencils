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

package exastencils.core

import exastencils.datastructures.Transformation.convFromNode
import exastencils.datastructures._

object NodeCounter extends CustomStrategy("internal::NodeCounter") {
  var iteration = 0
  var hits = 0
  var t = new Transformation("Count", { case x => hits += 1; x })

  println("nodecounter;strategy;transformation;iteration;nodes\\\\")

  def count(strategy : Option[String], transformation : Option[String]) : Unit = {
    iteration += 1
    hits = 0
    StateManager.applyStandalone(this, t, StateManager.root)

    var sb = new StringBuilder()
    sb.append("nodecounter;")
    sb.append(strategy.getOrElse("unknown").replace(" ", "\\_"))
    sb.append(';')
    sb.append(transformation.getOrElse("unknown").replace(" ", "\\_"))
    sb.append(';')
    sb.append(iteration)
    sb.append(';')
    sb.append(hits)
    sb.append("\\\\")
    println(sb.toString)
  }
  def count(strategy : String, transformation : String) : Unit = count(Some(strategy), Some(transformation))
  def count(strategy : String) : Unit = count(Some(strategy), None)

  def apply() = count(None, None)

  def countSubTree(node : Node, subTreeLabel : String, strategy : Option[String], transformation : Option[String]) : Unit = {
    iteration += 1
    hits = 0
    StateManager.applyStandalone(this, t, node)

    var sb = new StringBuilder()
    sb.append("nodecounter;")
    sb.append(subTreeLabel)
    sb.append(';')
    sb.append(strategy.getOrElse("unknown").replace(" ", "\\_"))
    sb.append(';')
    sb.append(transformation.getOrElse("unknown").replace(" ", "\\_"))
    sb.append(';')
    sb.append(iteration)
    sb.append(';')
    sb.append(hits)
    sb.append("\\\\")
    println(sb.toString)
  }

  def resetHits() = {
    hits = 0
  }

  def reset() = {
    iteration = 0
    hits = 0
  }

}