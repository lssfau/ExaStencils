package exastencils.datastructures.l3

trait ProgressableToL4 {
  /** Transform to destination code. */
  def toTc(env : Environment) : TargetCode
}