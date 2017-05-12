package exastencils.base.l1

import exastencils.logger._
import exastencils.config._
import exastencils.base.l1._

case class L1_Domain(var identifier : String, var range : List[Tuple2[Double, Double]]) extends L1_Definition {
  range.length match {
    case (1 | 2 | 3) => Knowledge.dimensionality = range.length
    case _           => Logger.error(s"""Dimensionality of ${range.length} not supported.""");
  }
}
