package exastencils.communication

import scala.collection.mutable.ListBuffer

import exastencils.config.Knowledge
import exastencils.deprecated.ir.IR_DimToString
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

    if (Knowledge.comm_onlyAxisNeighbors) {
      var neighIndex = 0
      for (dim <- 0 until Knowledge.dimensionality) {
        neighbors += NeighborInfo(Array.fill(dim)(0) ++ Array(-1) ++ Array.fill(Knowledge.dimensionality - dim - 1)(0), neighIndex)
        neighIndex += 1
        neighbors += NeighborInfo(Array.fill(dim)(0) ++ Array(+1) ++ Array.fill(Knowledge.dimensionality - dim - 1)(0), neighIndex)
        neighIndex += 1
      }
    } else {
      val unitDirections = Array(-1, 0, 1)
      var directions = ListBuffer(ListBuffer(-1), ListBuffer(0), ListBuffer(1))
      for (dim <- 1 until Knowledge.dimensionality)
        directions = for (dir <- directions; newComponent <- unitDirections) yield dir :+ newComponent

      var neighIndex = 0
      for (dir <- directions; if dir.map(i => if (0 == i) 0 else 1).sum > 0) {
        neighbors += NeighborInfo(dir.toArray, neighIndex)
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

case class NeighborInfo(var dir : Array[Int], var index : Int) {
  def dirToString(dir : Int) : String = {
    if (dir < 0)
      Array.fill(dir)("N").mkString
    else if (dir > 0)
      Array.fill(dir)("P").mkString
    else
      "0"
  }

  def label = (Knowledge.dimensionality - 1 to 0 by -1).toList.map(i => IR_DimToString(i).toUpperCase + dirToString(dir(i))).mkString("_")
}
