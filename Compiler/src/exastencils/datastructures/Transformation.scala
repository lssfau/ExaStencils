package exastencils.datastructures

import exastencils.core.StateManager
import scala.collection.GenTraversableOnce
import scala.collection.mutable.ListBuffer

// FIXME extend this to PartialFunction[Any(Ref), Transformation.Output[_]]
class Transformation(val name : String, val function : PartialFunction[Node, Transformation.Output[_]], val recursive : Boolean = true, val applyAtNode : Option[Node] = None) {
  override def toString() = s"""Transformation "$name""""
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
  class Output[T](val inner : T)(implicit val ev : T => Node Or NodeList)

  implicit def convFromSome[O <: Node](o : Some[O]) : Output[O] = new Output(o.get)
  implicit def convFromNode[O <: Node](o : O) : Output[O] = new Output(o)
  // FIXME: the next line is currently tested, the 2 lines after that should be obsolete now
  implicit def convFromList[N <: Node, L[X] <: GenTraversableOnce[X]](o : L[N]) : Output[NodeList] = new Output(new NodeList(o))
  //implicit def convFromList(o : List[Node]) : Output[NodeList] = new Output(new NodeList(o))
  //implicit def convFromListBuffer(o : ListBuffer[Node]) : Output[NodeList] = new Output(new NodeList(o))

  object Output {
    def apply[T](inner : T)(implicit ev : T => Node Or NodeList) = new Output(inner)
  }

  def apply(name : String, function : PartialFunction[Node, Output[_]], recursive : Boolean = true, applyAtNode : Option[Node] = None) =
    new Transformation(name, function, recursive, applyAtNode)
}

class TransformationResult(val successful : Boolean, val matches : Int) {
  override def toString() = {
    var s = "Transformation Result: "
    if (!successful) s += "not "
    s += s"successful, $matches matches"
    s
  }
}
