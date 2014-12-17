package exastencils.datastructures.l3

trait ProgressibleToL4 {
  /** Transform to destination code. */
  def toDc(env: Environment) : DestinationCode
}