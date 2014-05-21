package exastencils.parsers

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

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

  lazy val datatype : Parser[Datatype] = (
    simpleDatatype
    ||| numericDatatype
    ||| "Array" ~ ("[" ~> datatype <~ "]") ~ ("[" ~> integerLit <~ "]") ^^ { case _ ~ x ~ s => new ArrayDatatype(x, s) })

  lazy val simpleDatatype : Parser[Datatype] = (
    "String" ^^ { case x => new StringDatatype }
    ||| numericSimpleDatatype)

  lazy val numericDatatype : Parser[Datatype] = (
    ("Complex" ~ "[") ~> numericSimpleDatatype <~ "]" ^^ { case x => new ComplexDatatype(x) }
    ||| numericSimpleDatatype)

  lazy val numericSimpleDatatype : Parser[Datatype] = (
    "Integer" ^^ { case x => new IntegerDatatype }
    ||| "Real" ^^ { case x => new RealDatatype })

  lazy val returnDatatype = ("Unit" ^^ { case x => new UnitDatatype }
    ||| datatype)

  lazy val integerLit = (
    numericLit ^^ { case n if (isInt(n)) => n.toInt }
    ||| ("-" ~> numericLit ^^ { case n if (isInt(n)) => -n.toInt }))

  lazy val realLit = (
    numericLit ^^ { case n if (isReal(n)) => n.toDouble }
    ||| ("-" ~> numericLit ^^ { case n if (isReal(n)) => -n.toDouble }))

  lazy val booleanLit : Parser[String] = "true" ||| "false"

  // set members of given object obj via reflection
  def set[T](obj : AnyRef, ident : String, value : T) = {
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
