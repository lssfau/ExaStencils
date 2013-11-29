package exastencils.parsers

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

class ExaParser extends JavaTokenParsers {
  override def skipWhitespace = true
  //  def newline: Parser[Any] = "\n" | "\r\n"
  //  def listdelimiter: Parser[Any] = newline | ","

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
