package exastencils.math

import scala.language.postfixOps

/** Representation of a general stencil. */
class Stencil[T](entries : List[ (T, Vector[Int])]) {
  assert(entries forall { case (_, v) => v.rows == entries.head._2.rows } )
  
  def withoutCenter() = {
    new Stencil( entries filter { case (_, off) => !(off isZero) } )
  }
  
  def center() = {
    new Stencil( entries filter {case (_, off) => off isZero })
  }
}