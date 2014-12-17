package exastencils.datastructures.l3

trait ProgressableToL4 {
  def progressToL4 : Any = {
    throw new Exception("Not implemented")
  }

  /** Transform to destination code. */
  def toDc(env : Environment) : DestinationCode
}