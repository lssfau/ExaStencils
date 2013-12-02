package exastencils.parsers

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

import exastencils.datastructures.l4._

class ExaParser extends StandardTokenParsers {
  override val lexical : ExaLexer = new ExaLexer()
  def listdelimiter = newline | ","
  def newline = "\n" | "\r\n"
  
  def datatype : Parser[Datatype] = simpleDatatype |
    numericDatatype |
    { "Array" ~ "(" } ~> datatype <~ ")" ^^ { case x => new ArrayDatatype(x) }

  def simpleDatatype = "Unit" ^^ { case x => new UnitDatatype } |
    "String" ^^ { case x => new StringDatatype } |
    numericSimpleDatatype

  def numericDatatype = "Complex" ~> numericSimpleDatatype <~ ")" ^^ { case x => new ComplexDatatype(x) } |
    numericSimpleDatatype

  def numericSimpleDatatype = "Integer" ^^ { case x => new IntegerDatatype } |
    "Real" ^^ { case x => new RealDatatype }

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
