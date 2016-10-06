package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.config.Knowledge
import exastencils.deprecated.ir.IR_DimToString
import exastencils.logger._

class NeighborInfo(var dir : Array[Int], var index : Int) {
  var label : String = (Knowledge.dimensionality - 1 to 0 by -1).toList.map(i => IR_DimToString(i).toUpperCase + dirToString(dir(i))).mkString("_")
}

object dirToString extends (Int => String) {
  def apply(dim : Int) : String = {
    return dim match {
      case -1 => "N"
      case 0  => "0"
      case 1  => "P"
      case _  => "UNKNOWN"
    }
  }
}

object Fragment {
  var neighbors : ListBuffer[NeighborInfo] = ListBuffer()

  // ignores array entries beyond Knowledge.dimensionality
  def getNeigh(dir : Array[Int]) : NeighborInfo = {
    if (dir.size >= Knowledge.dimensionality) {
      for (neigh <- neighbors) {
        if ((0 until Knowledge.dimensionality).map(i => dir(i) == neigh.dir(i)).reduce((a, b) => a && b))
          return neigh
      }
    }
    Logger.warn("Trying to access invalid neighbor: " + dir.mkString(", "))
    neighbors(0)
  }

  def getOpposingNeigh(index : Int) : NeighborInfo = getOpposingNeigh(neighbors(index))
  def getOpposingNeigh(neigh : NeighborInfo) = getNeigh(neigh.dir.map(i => -i))

  def setupNeighbors() : Unit = {
    neighbors.clear

    Knowledge.comm_strategyFragment match {
      case 6  => {
        var neighIndex = 0
        for (dim <- 0 until Knowledge.dimensionality) {
          neighbors += new NeighborInfo(Array.fill(dim)(0) ++ Array(-1) ++ Array.fill(Knowledge.dimensionality - dim - 1)(0), neighIndex)
          neighIndex += 1
          neighbors += new NeighborInfo(Array.fill(dim)(0) ++ Array(+1) ++ Array.fill(Knowledge.dimensionality - dim - 1)(0), neighIndex)
          neighIndex += 1
        }
      }
      case 26 => {
        val unitDirections = Array(-1, 0, 1)
        var directions = ListBuffer(ListBuffer(-1), ListBuffer(0), ListBuffer(1))
        for (dim <- 1 until Knowledge.dimensionality)
          directions = (for (dir <- directions; newComponent <- unitDirections) yield (dir :+ newComponent))

        var neighIndex = 0
        for (dir <- directions; if (dir.map(i => if (0 == i) 0 else 1).reduce(_ + _) > 0)) {
          neighbors += new NeighborInfo(dir.toArray, neighIndex)
          neighIndex += 1
        }
      }
    }

    // FIXME: remove HACK after fragment positions, etc are stored with correct data types
    if (Knowledge.dimensionality < 3) {
      for (neigh <- neighbors) { neigh.dir ++= Array.fill(3 - Knowledge.dimensionality)(0) }
    }
  }
}
