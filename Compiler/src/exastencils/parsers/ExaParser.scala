package exastencils.parsers

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

import exastencils.datastructures._
import exastencils.datastructures.l4._

class ExaParser extends StandardTokenParsers with scala.util.parsing.combinator.PackratParsers {
  override val lexical : ExaLexer = new ExaLexer()

  lazy val listdelimiter = newline | ","
  lazy val newline = "\n" | "\r\n"

  lazy val datatype : Parser[Datatype] =
    simpleDatatype |
      numericDatatype |
      { "Array" ~ "(" } ~> datatype <~ ")" ^^ { case x => new ArrayDatatype(x) }

  lazy val simpleDatatype : Parser[Datatype] =
    "Unit" ^^ { case x => new UnitDatatype } |
      "String" ^^ { case x => new StringDatatype } |
      numericSimpleDatatype

  lazy val numericDatatype : Parser[Datatype] =
    "Complex" ~> numericSimpleDatatype <~ ")" ^^ { case x => new ComplexDatatype(x) } |
      numericSimpleDatatype

  lazy val numericSimpleDatatype : Parser[Datatype] =
    "Integer" ^^ { case x => new IntegerDatatype } |
      "Real" ^^ { case x => new RealDatatype }

  lazy val literal : Parser[Expression] = stringLit ^^ { case x => StringLiteral(x) } |
    numericLit ^^ { case x => NumericLiteral(x.toDouble) } | // FIXME split into integerLiteral and realLiteral
    booleanLit ^^ { case x => BooleanLiteral(x.toBoolean) }

  lazy val booleanLit : Parser[String] = "true" | "false"

  // set members of given object obj via reflection
  def set[T](obj : AnyRef, ident : String, value : T) {
    if (!value.equals(None))
      println("setting " + ident + " to " + value)
    else
      println("variable " + ident)
    val field = obj.getClass.getDeclaredField(ident)
    val accessible = field.isAccessible
    field.setAccessible(true)

    if (field.get(obj).getClass.equals(None.getClass())) {
      // Field is Option[T]
      obj.getClass.getMethods.find(_.getName == ident + "_$eq").get.invoke(obj, Option[T](value))
    } else {
      // field is POSO - set directly
      obj.getClass.getMethods.find(_.getName == ident + "_$eq").get.invoke(obj, value.asInstanceOf[Object])
    }

    field.setAccessible(accessible)
  }

}
