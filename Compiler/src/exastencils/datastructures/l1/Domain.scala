package exastencils.datastructures.l1

import exastencils.logger._
import exastencils.knowledge._
import exastencils.datastructures.l1._

case class Domain(var identifier : String, var range : List[Tuple2[Double, Double]]) extends Definition {
  range.length match {
    case (1 | 2 | 3) => Knowledge.dimensionality = range.length
    case _           => Logger.error(s"""Dimensionality of ${range.length} not supported.""");
  }
}
