package exastencils.parsers

import scala.collection.mutable.Stack
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

import exastencils.datastructures._

class ExaParser extends StandardTokenParsers with PackratParsers {
  val IntRegEx = """[+-]?(\d+)""".r
  val DoubleRegEx = """[+-]?\d+(\.\d*)?""".r

  var filenames = Stack[String]()

  protected def isInt(str : String) : Boolean = {
    val x = IntRegEx.findFirstIn(str)
    x.getOrElse(" ") == str
  }

  protected def isReal(str : String) : Boolean = {
    val x = DoubleRegEx.findFirstIn(str)
    x.getOrElse(" ") == str
  }

  def locationize[T <: Annotatable](p : => Parser[T]) : Parser[T] = Parser { in =>
    p(in) match {
      case hit @ Success(t : Node, _) =>
        t.location.position = Some(in.pos)
        if (filenames.nonEmpty)
          t.location.fileName = Some(filenames.top)

        hit

      case other => other
    }
  }

  lazy val listdelimiter = ","

  lazy val literal = (
    stringLit
      ||| "-".? ~ numericLit ^^ {
      case s ~ n if isInt(s.getOrElse("") + n) => (s.getOrElse("") + n).toInt : AnyVal
      case s ~ n                               => (s.getOrElse("") + n).toDouble : AnyVal
    }
      ||| booleanLit)

  lazy val integerLit = (
    numericLit ^^ { case n if isInt(n) => n.toInt }
      ||| ("-" ~> numericLit ^^ { case n if isInt(n) => -n.toInt }))

  lazy val realLit = (
    numericLit ^^ { case n if isReal(n) => n.toDouble }
      ||| ("-" ~> numericLit ^^ { case n if isReal(n) => -n.toDouble }))

  lazy val booleanLit : Parser[Boolean] = ("true" ||| "false") ^^ (b => b == "true")
}
