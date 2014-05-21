package exastencils.math

import scala.language.postfixOps

object GeneralStencil {
  def apply[T](entries : (T, Vector[Int])*) = 
    new GeneralStencil[T](entries.toList)
  
}

/** Representation of a general stencil. */
class GeneralStencil[T](entries : List[ (T, Vector[Int])]) {
  assert(entries forall { case (_, v) => v.rows == entries.head._2.rows } )
  
  def withoutCenter() = {
    new GeneralStencil( entries filter { case (_, off) => !(off isZero) } )
  }
  
  def center() = {
    new GeneralStencil( entries filter {case (_, off) => off isZero })
  }
}
