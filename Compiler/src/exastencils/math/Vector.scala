package exastencils.math

object Vector {
  
  /** Creates the zero vector. */
  def zero[T](rows : Int)(implicit mf : ClassManifest[T], num : Numeric[T]) =
    new Vector[T](new Array[T](rows))
  
  /** Creates a constant vector. */
  def constant[T](rows : Int, value : T)(implicit tag : ClassManifest[T], num : Numeric[T]) =
    new Vector[T](Array.fill(rows)(value))
  
  /** Creates a vector containing ones. */
  def ones[T](rows : Int)(implicit tag : ClassManifest[T], num : Numeric[T]) =
    constant[T](rows, num.fromInt(1))
}

/** Numerical vector. */
class Vector[T] (private val elements : Array[T])(implicit mf : ClassManifest[T], num : Numeric[T]) {
  import num._
  
  def rows = elements.length
  
  /** Apply an element wise operation to two vectors and return a new one. */
  def elementwise(rhs : Vector[T])(fun: (T, T) => T) : Vector[T] = {
    assert(rows == rhs.rows) 
    
    val rv = new Array[T](rows)
    for (i <- Range(0, rows)) {
      rv(i) = fun(elements(i), rhs.elements(i))
    }
    new Vector[T](rv)
  }
  
  /** Apply an element wise operator to the vector and return a new one. */
  def elementwise(fun: T => T) : Vector[T] = {
    new Vector[T](elements map fun)
  }
  
  def +(rhs : Vector[T]) = elementwise(rhs) { (x,y) => x+y }
  def -(rhs : Vector[T]) = elementwise(rhs) { (x,y) => x-y }
  
  def ==(rhs : Vector[T]) =
    (elements zip rhs.elements) forall { case (x, y) => x == y }
  def !=(rhs : Vector[T]) = !(this == rhs)
  
  def isZero = elements forall {_ == num.fromInt(0)}
}

object Implicits {
  implicit def Numeric2Scalar[T](value : T)(implicit num : Numeric[T]) = 
    new Scalar[T](value)
}

/** Helper class that provides scalar times vector multiplication. */
class Scalar[T] (private val value : T)(implicit num : Numeric[T]) {
  import num._
  
  def *(rhs : Vector[T]) = rhs elementwise { x => value * x }
}
