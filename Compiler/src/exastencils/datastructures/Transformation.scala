package exastencils.datastructures

import exastencils.core.StateManager
import scala.collection.GenTraversableOnce
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

// FIXME extend this to PartialFunction[Any(Ref), Transformation.Output[_]]
class Transformation(val name : String, val function : PartialFunction[Node, Transformation.OutputType], val recursive : Boolean = true, val applyAtNode : Option[Node] = None) {
  override def toString() = s"""Transformation "$name""""
}

object Transformation {
  import scala.language.implicitConversions

  // workaround since Scala's type system does not allow for real union types
  type Or[A, B] = Either[A, B]
  class Output[T <: AnyRef](val inner : T)(implicit val ev : T => Node Or NodeList Or Option[Nothing])

  // implicit defs for workaround
  implicit def l[T](t : T) = Left(t)
  implicit def r[T](t : T) = Right(t)
  implicit def ll[T](t : T) = Left(Left(t))
  implicit def lr[T](t : T) = Left(Right(t))
  implicit def lll[T](t : T) = Left(Left(Left(t)))
  implicit def llr[T](t : T) = Left(Left(Right(t)))
  implicit def llll[T](t : T) = Left(Left(Left(Left(t))))
  implicit def lllr[T](t : T) = Left(Left(Left(Right(t))))

  // implicit conversions to specify Transformations more elegantly
  implicit def convFromSome[O <: Node](o : Some[O]) : Output[O] = new Output(o.get)
  implicit def convFromNode[O <: Node](o : O) : Output[O] = new Output(o)
  implicit def convFromNone(o : Option[Nothing]) : Output[Option[Nothing]] = new Output(o)
  implicit def convFromList[N <: Node, L[X] <: GenTraversableOnce[X]](o : L[N]) : Output[NodeList] = new Output(new NodeList(o))

  type OutputType = Output[_ <: AnyRef]

  object Output {
    def apply[T <: AnyRef](inner : T)(implicit ev : T => Node Or NodeList Or Option[Nothing]) = new Output(inner)
  }

  def apply(name : String, function : PartialFunction[Node, OutputType], recursive : Boolean = true, applyAtNode : Option[Node] = None) = {
    new Transformation(name, function, recursive, applyAtNode)
  }
}

class TransformationResult(val successful : Boolean, val matches : Int) {
  override def toString() = {
    var s = "Transformation Result: "
    if (!successful) s += "not "
    s += s"successful, $matches matches"
    s
  }
}
