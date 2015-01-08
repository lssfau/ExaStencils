package exastencils.parsers

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

import exastencils.datastructures._
import exastencils.datastructures.l4._

class ExaParser extends StandardTokenParsers {
  override val lexical : ExaLexer = new ExaLexer()

  val IntRegEx = """[+-]?(\d+)""".r
  val DoubleRegEx = """[+-]?\d+(\.\d*)?""".r
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
      case Success(t, in1) => Success(if (!t.hasAnnotation("location")) { t.add(new Annotation("location", Some(in.pos))); t } else t, in1)
      case ns : NoSuccess  => ns
    }
  }

  lazy val listdelimiter = newline | ","
  lazy val newline = "\n" | "\r\n"

  lazy val integerLit = (
    numericLit ^^ { case n if (isInt(n)) => n.toInt }
    ||| ("-" ~> numericLit ^^ { case n if (isInt(n)) => -n.toInt }))

  lazy val realLit = (
    numericLit ^^ { case n if (isReal(n)) => n.toDouble }
    ||| ("-" ~> numericLit ^^ { case n if (isReal(n)) => -n.toDouble }))

  lazy val booleanLit : Parser[String] = "true" ||| "false"
}
