package exastencils.grid

import scala.collection.mutable.ListBuffer

/// GridGeometry_uniform_nonStaggered_AA

object GridGeometry_uniform_nonStaggered_AA extends GridGeometry_uniform {
  // nothing else to do here since everything can be pre-computed/ inlined
  override def initL4() = {}
  override def generateInitCode() = ListBuffer()
}

