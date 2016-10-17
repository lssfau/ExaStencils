package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.config.Knowledge
import exastencils.logger._

/// GridGeometry_nonUniform_nonStaggered_AA

object GridGeometry_nonUniform_nonStaggered_AA extends GridGeometry_nonUniform {
  override def generateInitCode() = {
    Knowledge.grid_spacingModel match {
      case "uniform"   =>
        (Knowledge.maxLevel to Knowledge.minLevel by -1).map(level =>
          Knowledge.dimensions.to[ListBuffer].flatMap(dim => setupNodePos_Uniform(dim, level)))
          .reduceLeft(_ ++ _)
      case "linearFct" =>
        Logger.warn("LinearFct spacing model is currently not recommended for GridGeometry_nonUniform_nonStaggered_AA since grid point positions won't match across levels")
        (Knowledge.maxLevel to Knowledge.minLevel by -1).map(level =>
          Knowledge.dimensions.to[ListBuffer].flatMap(dim => setupNodePos_LinearFct(dim, level)))
          .reduceLeft(_ ++ _)
    }
  }
}
