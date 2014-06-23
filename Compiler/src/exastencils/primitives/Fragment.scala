package exastencils.primitives

import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._
import exastencils.prettyprinting._

case class FragmentClass(var neighbors : ListBuffer[NeighborInfo] = ListBuffer()) extends Node {
  def setupNeighbors() : Unit = {
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
