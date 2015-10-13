package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.logger._

class NeighborInfo(var dir : Array[Int], var index : Int) {
  var label : String = (Knowledge.dimensionality - 1 to 0 by -1).toList.map(i => dimToString(i).toUpperCase + dirToString(dir(i))).mkString("_")
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

  /// ignores array entries beyond Knowledge.dimensionality
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

  def setupNeighbors() : Unit = {
    neighbors.clear

    Knowledge.comm_strategyFragment match {
      case 6 => {
        neighbors += new NeighborInfo(Array(-1, 0, 0), 0)
        neighbors += new NeighborInfo(Array(+1, 0, 0), 1)
        if (Knowledge.dimensionality > 1) {
          neighbors += new NeighborInfo(Array(0, -1, 0), 2)
          neighbors += new NeighborInfo(Array(0, +1, 0), 3)
        }
        if (Knowledge.dimensionality > 2) {
          neighbors += new NeighborInfo(Array(0, 0, -1), 4)
          neighbors += new NeighborInfo(Array(0, 0, +1), 5)
        }
      }
      case 26 => {
        var i = 0
        for (
          z <- (if (Knowledge.dimensionality > 2) (-1 to 1) else (0 to 0));
          y <- (if (Knowledge.dimensionality > 1) (-1 to 1) else (0 to 0));
          x <- -1 to 1;
          if (0 != x || 0 != y || 0 != z)
        ) {
          neighbors += new NeighborInfo(Array(x, y, z), i)
          i += 1
        }
      }
    }
  }
}
