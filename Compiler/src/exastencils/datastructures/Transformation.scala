package exastencils.datastructures

import exastencils.core.StateManager

class Transformation(n : String, f : PartialFunction[Node, Transformation.Output[_]], rec : Boolean = true, node : Option[Node] = None) {
  def name = n
  def function = f
  def recursive = rec
  def applyAt = node
}

object Transformation {
  import scala.language.implicitConversions

  type Or[A, B] = Either[A, B]

  // implicit defs 
  implicit def l[T](t : T) = Left(t)
  implicit def r[T](t : T) = Right(t)
  implicit def ll[T](t : T) = Left(Left(t))
  implicit def lr[T](t : T) = Left(Right(t))
  implicit def lll[T](t : T) = Left(Left(Left(t)))
  implicit def llr[T](t : T) = Left(Left(Right(t)))
  implicit def llll[T](t : T) = Left(Left(Left(Left(t))))
  implicit def lllr[T](t : T) = Left(Left(Left(Right(t))))

  // workaround since Scala's type system does not allow for real union types
  class Output[T <% Node Or List[Node]](val inner : T)
  implicit def convFromSome[O <: Node](o : Some[O]) : Output[O] = new Output(o.get)

  object Output {
    def apply[T <% Node Or List[Node]](inner : T) = new Output(inner)
  }

  def apply(n : String, f : PartialFunction[Node, Output[_]], rec : Boolean = true, node : Option[Node] = None) =
    new Transformation(n, f, rec, node)
}

class TransformationResult(val successful : Boolean, val matches : Int, val replacements : Int) {
  override def toString() = {
    var s = "Transformation Result: "
    if (!successful) s += "not "
    s += s"successful, $matches matches, $replacements replacements"
    s
  }
}
