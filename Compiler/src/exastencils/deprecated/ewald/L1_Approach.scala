package exastencils.deprecated.ewald

/** basic interface for a finite difference approach
  *
  * getWeights() is used to get the finite difference weights
  *
  * @param  N number of gridPoints
  * @param  M order of derivative
  * @param  h distance of neighboring grid points
  */
abstract class L1_Approach(val N : Int, val M : Int, val h : Double) {
  assert(N >= 1)
  assert(M >= 0)
  assert(h > 0)
  def getWeights() : List[Double]
}
