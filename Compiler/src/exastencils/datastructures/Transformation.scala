package exastencils.datastructures

import exastencils.core.StateManager
import scala.collection.GenTraversableOnce
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

/**
  * The class to specify a transformation modifying a program state into a new one.
  *
  * @param name The name of the Transformation. Used for traceability and debugging purposes.
  * @param function A Scala function specifying the nodes to look for and specifying their replacement.
  * @param recursive Optional; specifies if the Transformation is to be applied to subnodes of a node that has just been replaced. Default value is true.
  * @param applyAtNode Optional; specifies a source node where the Transformation starts to traverse the program state.
  */
class Transformation(val name : String, val function : PartialFunction[Node, Transformation.OutputType], val recursive : Boolean = true, val applyAtNode : Option[Node] = None) {
  override def toString() = s"""Transformation "$name""""
}

/**
  * Companion object for the Transformation class.
  */
object Transformation {
  import scala.language.implicitConversions

  // workaround since Scala's type system does not allow for real union types
  type Or[A, B] = Either[A, B]

  /**
    * The result type for Transformation functions.
    */
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

  /**
    * Companion object for the Transformation.Output class.
    */
  object Output {
    def apply[T <: AnyRef](inner : T)(implicit ev : T => Node Or NodeList Or Option[Nothing]) = new Output(inner)
  }

  /**
    * Creates a new Transformation which, for example, can be added to a Strategy.
    *
    * @param name The name of the transformation. Used for debugging purposes.
    * @param function A Scala function specifying the nodes to look for and specifying their replacement.
    * @param recursive Specifies if the Transformation is to be applied to subnodes of a node that has just been replaced.
    * @param applyAtNode Specifies the source node where the transformation starts to traverse the program state.
    */
  def apply(name : String, function : PartialFunction[Node, OutputType], recursive : Boolean = true, applyAtNode : Option[Node] = None) = {
    new Transformation(name, function, recursive, applyAtNode)
  }
}

/**
 * Represents some statistics about a Transformation.
 * 
 * @param successful Specifies if the Transformation did finish successfully (independent of the number of matches) or was aborted.
 * @param matches The number of matches - and nodes replaced - by the Transformation.
 */
class TransformationResult(val successful : Boolean, val matches : Int) {
  override def toString() = {
    var s = "Transformation Result: "
    if (!successful) s += "not "
    s += s"successful, $matches matches"
    s
  }
}
