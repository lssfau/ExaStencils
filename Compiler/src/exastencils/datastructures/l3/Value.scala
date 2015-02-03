package exastencils.datastructures.l3

trait Value {
  def scType : ScType
}

trait LValue {
  /** Returns the corresponding r-value. */
  def deref : RValue
}

trait RValue {

}

