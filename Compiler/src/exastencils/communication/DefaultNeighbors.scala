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

package exastencils.communication

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.config.Knowledge
import exastencils.logger.Logger

/// DefaultNeighbors

object DefaultNeighbors {
  var neighbors = ListBuffer[NeighborInfo]()

  // ignores array entries beyond Knowledge.dimensionality
  def getNeigh(dir : Array[Int]) : NeighborInfo = {
    if (dir.length >= Knowledge.dimensionality) {
      for (neigh <- neighbors) {
        if (Knowledge.dimensions.map(i => dir(i) == neigh.dir(i)).reduce((a, b) => a && b))
          return neigh
      }
    }
    Logger.warn("Trying to access invalid neighbor: " + dir.mkString(", "))
    neighbors(0)
  }

  def getOpposingNeigh(index : Int) : NeighborInfo = getOpposingNeigh(neighbors(index))
  def getOpposingNeigh(neigh : NeighborInfo) = getNeigh(neigh.dir.map(i => -i))

  def setup() : Unit = {
    neighbors.clear

    val equalLevelNeighbors = HashMap(/* levelDiff */ 0 -> /* numNeighbors */ 1)

    if (Knowledge.comm_onlyAxisNeighbors) {
      var neighIndex = 0
      for (dim <- 0 until Knowledge.dimensionality) {
        val downwindDir = Array.fill(dim)(0) ++ Array(-1) ++ Array.fill(Knowledge.dimensionality - dim - 1)(0)
        val upwindDir   = Array.fill(dim)(0) ++ Array(+1) ++ Array.fill(Knowledge.dimensionality - dim - 1)(0)

        val neighborsPerRefinementCase = if (Knowledge.refinement_enabled)
          // equal level + fine-to-coarse (1 neighbor) + coarse-to-fine (multiple neighbors)
          equalLevelNeighbors + (-1 -> 1) + (1 -> Knowledge.refinement_maxCommNeighborsPerDir)
        else
          // no refinement -> equal level
          equalLevelNeighbors

        neighbors += NeighborInfo(downwindDir, neighborsPerRefinementCase, neighIndex)
        neighIndex += 1
        neighbors += NeighborInfo(upwindDir, neighborsPerRefinementCase, neighIndex)
        neighIndex += 1
      }
    } else {
      // TODO: only equal-level communication supported
      val unitDirections = Array(-1, 0, 1)
      var directions = ListBuffer(ListBuffer(-1), ListBuffer(0), ListBuffer(1))
      for (dim <- 1 until Knowledge.dimensionality)
        directions = for (dir <- directions; newComponent <- unitDirections) yield dir :+ newComponent

      var neighIndex = 0
      for (dir <- directions; if dir.map(i => if (0 == i) 0 else 1).sum > 0) {
        neighbors += NeighborInfo(dir.toArray, equalLevelNeighbors, neighIndex)
        neighIndex += 1
      }
    }

    // FIXME: remove HACK after fragment positions, etc are stored with correct data types
    if (Knowledge.dimensionality < 3) {
      for (neigh <- neighbors) { neigh.dir ++= Array.fill(3 - Knowledge.dimensionality)(0) }
    }
  }
}

/// NeighborInfo

case class NeighborInfo(var dir : Array[Int], numNeighborsForRefinementCase : HashMap[Int, Int], var index : Int) {
  def dirToString(dir : Int) : String = {
    if (dir < 0)
      Array.fill(dir)("N").mkString
    else if (dir > 0)
      Array.fill(dir)("P").mkString
    else
      "0"
  }

  def label = (Knowledge.dimensionality - 1 to 0 by -1).toList.map(i => s"i$i" + dirToString(dir(i))).mkString("_")
}
